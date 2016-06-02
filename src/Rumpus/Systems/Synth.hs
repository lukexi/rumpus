module Rumpus.Systems.Synth
    ( module Rumpus.Systems.Synth
    , module Sound.Pd
    ) where

import PreludeExtra
import Sound.Pd

import Rumpus.Systems.Shared
import Rumpus.Systems.Scene
import Rumpus.Systems.Controls
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

type DACChannel = Int
-- Keep track of which file the patch was derived from so we
-- can avoid unncessarily reloading it
data PatchWithFile = PatchWithFile
    { pwfFilePath :: !FilePath
    , pwfPatch    :: !Patch
    , pwfIsPoly   :: !Bool
    }

data AllocatedOpenALSource = AllocatedOpenALSource
    { aosEntityID     :: !EntityID
    , aosDACChannel   :: !DACChannel
    , aosOpenALSource :: !OpenALSource
    }

data PolyPatchVoice = PolyPatchVoice
    { ppvPatch        :: !PatchWithFile
    , ppvOpenALSource :: !AllocatedOpenALSource
    }

data SynthSystem = SynthSystem
    { _sndPd               :: !PureData
    , _sndOpenALSourcePool :: ![AllocatedOpenALSource]
    , _sndPolyPatchVoices  :: !(Map FilePath [PolyPatchVoice])
    }

makeLenses ''SynthSystem
defineSystemKey ''SynthSystem

defineComponentKeyWithType "PdPatch"     [t|PatchWithFile|]
defineComponentKeyWithType "PdPatchFile" [t|FilePath|]

defineComponentKey ''OpenALSource

-- | The number of patches to acquire
polyPatchPolyphony :: Int
polyPatchPolyphony = 8

-- | Returns a list of instances of a Pd patch with the given name.
-- To create this list, create N instances (where N=polyPatchPolyphony)
-- and steal N voices from the global OpenAL allocator. These will be
-- permanently assigned to the polyphonic voices.
getPolyPatchVoices :: (MonadState ECS m, MonadIO m) => FilePath -> m [PolyPatchVoice]
getPolyPatchVoices patchName = do
    viewSystem sysSynth (sndPolyPatchVoices . at patchName) >>= \case
        Just polyPatchVoices -> return polyPatchVoices
        Nothing -> do
            patches <- replicateM polyPatchPolyphony $ makePatchWithFile patchName True
            sources <- stealOpenALSources polyPatchPolyphony

            pd <- getPd
            forM_ (zip patches (map aosDACChannel sources)) $ \(patch, sourceChannel) ->
                send pd (pwfPatch patch) "dac" (fromIntegral sourceChannel)
            let polyPatchVoices = zipWith PolyPatchVoice
                                            patches
                                            sources
            modifySystemState sysSynth $
                sndPolyPatchVoices . at patchName ?= polyPatchVoices
            return polyPatchVoices

releasePolyPatches :: (MonadIO m, MonadState ECS m) => m ()
releasePolyPatches = do
    pd <- getPd
    voicesMap <- viewSystem sysSynth sndPolyPatchVoices
    forM_ voicesMap $ \voices -> do
        forM_ voices $ \PolyPatchVoice{..} -> do
            _ <- closePatch pd (pwfPatch ppvPatch)

            -- Return the OpenAL source to the pool
            modifySystemState sysSynth $ do
                sndOpenALSourcePool %= (ppvOpenALSource { aosEntityID = 0 } : )
    modifySystemState sysSynth $ sndPolyPatchVoices .= mempty


acquirePolyPatch :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                 => FilePath -> m ()
acquirePolyPatch patchName = do

    thisEntityID <- ask

    currentPatch <- getComponent myPdPatch
    let alreadyHavePatch = fromMaybe False ((== patchName) . pwfFilePath <$> currentPatch)
    when (not alreadyHavePatch) $ do
        polyPatchVoices <- getPolyPatchVoices patchName
        case polyPatchVoices of
            [] -> return ()
            (voice:xs) -> do
                let PolyPatchVoice{..} = voice

                inEntity (aosEntityID ppvOpenALSource) $ do
                    removeComponent myOpenALSource
                    removeComponent myPdPatch

                inEntity thisEntityID $ do
                    myOpenALSource ==> aosOpenALSource ppvOpenALSource
                    myPdPatch      ==> ppvPatch

                let newVoice = voice { ppvOpenALSource = ppvOpenALSource { aosEntityID = thisEntityID } }
                modifySystemState sysSynth $
                    sndPolyPatchVoices . at patchName ?= xs ++ [newVoice]

getPd :: MonadState ECS m => m PureData
getPd = viewSystem sysSynth sndPd

addPdPatchSearchPath :: (MonadIO m, MonadState ECS m) => String -> m ()
addPdPatchSearchPath path = do
    pd <- viewSystem sysSynth sndPd
    addToLibPdSearchPath pd path

initSynthSystem :: (MonadState ECS m, MonadIO m) => PureData -> m ()
initSynthSystem pd = do
    mapM_ (addToLibPdSearchPath pd)
        ["resources/pd-kit", "resources/pd-kit/list-abs"]

    let soundSystem = SynthSystem
            { _sndPd = pd
            , _sndOpenALSourcePool = zipWith3 AllocatedOpenALSource
                (repeat 0 :: [EntityID])
                ([1..] :: [DACChannel])
                (pdSources pd)
            , _sndPolyPatchVoices = mempty
            }

    registerSystem sysSynth soundSystem

    -- No reason to save this right now while the only way to set a pd patch is via a start function
    --registerComponent "PdPatchFile" myPdPatchFile (savedComponentInterface myPdPatchFile)
    registerComponent "PdPatchFile" myPdPatchFile (newComponentInterface myPdPatchFile)
    registerComponent "OpenALSource" myOpenALSource (newComponentInterface myOpenALSource)
    registerComponent "PdPatch" myPdPatch $ (newComponentInterface myPdPatch)
        { ciDeriveComponent = Just derivePdPatchComponent
        , ciRemoveComponent = removePdPatchComponent
        }

tickSynthSystem :: (MonadIO m, MonadState ECS m) => m ()
tickSynthSystem = do
    headM44 <- getHeadPose
    -- Update source and listener positions
    alListenerPose (poseFromMatrix headM44)
    forEntitiesWithComponent myOpenALSource $ \(entityID, sourceID) -> do
        position <- view translation <$> getEntityPose entityID
        alSourcePosition sourceID position

-- | We currently implement this by voice stealing,
-- so the oldest object will lose sound.
dequeueOpenALSource :: (MonadState ECS m, MonadIO m) => EntityID -> m (Maybe AllocatedOpenALSource)
dequeueOpenALSource entityID = do
    openALSourcePool <- viewSystem sysSynth sndOpenALSourcePool
    case openALSourcePool of
        [] -> return Nothing
        (allocatedSource:xs) -> do
            inEntity (aosEntityID allocatedSource) $ do
                sendSynth "dac" 0
                removeComponent myOpenALSource
            let newAllocatedSource = allocatedSource { aosEntityID = entityID }
            modifySystemState sysSynth $ do
                sndOpenALSourcePool .= xs ++ [newAllocatedSource]
            return (Just newAllocatedSource)

stealOpenALSources :: (MonadState ECS m, MonadIO m) => Int -> m [AllocatedOpenALSource]
stealOpenALSources numSources = do
    stolenSources <- modifySystemState sysSynth $ do
        openALSourcePool <- use sndOpenALSourcePool
        sndOpenALSourcePool .= drop numSources openALSourcePool
        return (take numSources openALSourcePool)
    forM stolenSources $ \allocatedSource -> do
        inEntity (aosEntityID allocatedSource) $ do
            sendSynth "dac" 0
            removeComponent myOpenALSource
        return allocatedSource { aosEntityID = 0 }

setSynthPatch :: (MonadReader EntityID m, MonadState ECS m, MonadIO m)
               => FilePath
               -> m ()
setSynthPatch patchFile = do
    oldPatchFile <- getComponent myPdPatchFile
    when (oldPatchFile /= Just patchFile) $ do
        myPdPatchFile ==> patchFile
        derivePdPatchComponent

derivePdPatchComponent :: (MonadReader EntityID m, MonadState ECS m, MonadIO m)
                       => m ()
derivePdPatchComponent = do
    maybePatchFile <- getComponent myPdPatchFile
    case maybePatchFile of
        Nothing -> removePdPatchComponent
        Just patchFile -> do
            maybeExistingPatch <- getComponent myPdPatch

            -- Check if this entities patch is already loaded
            -- (i.e., we are being called to update an existing entity via the SceneWatcher)
            let alreadyLoaded = case maybeExistingPatch of
                    Just PatchWithFile{..} -> pwfFilePath == patchFile
                    Nothing                -> False
            unless alreadyLoaded $ do
                removePdPatchComponent

                patch <- makePatchWithFile patchFile False
                myPdPatch ==> patch

                -- Assign the patch's output DAC index to route it to the the SourceID
                entityID <- ask
                traverseM_ (dequeueOpenALSource entityID) $ \AllocatedOpenALSource{..} -> do
                    myOpenALSource ==> aosOpenALSource
                    putStrLnIO $ "loaded pd patch " ++ patchFile ++ ", assigning channel " ++ show aosDACChannel
                    sendSynth "dac" (fromIntegral aosDACChannel)

makePatchWithFile :: (MonadIO m, MonadState ECS m)
                  => FilePath
                  -> Bool
                  -> m PatchWithFile
makePatchWithFile patchFile isPoly = do
    pd <- getPd
    -- NOTE if we return to using scene folders to store patches, must use getSceneFolder here
    rumpusRoot <- getRumpusRootFolder
    addPdPatchSearchPath rumpusRoot
    patch <- makePatch pd (rumpusRoot </> takeBaseName patchFile)
    return PatchWithFile { pwfFilePath = patchFile, pwfPatch = patch, pwfIsPoly = isPoly }

removePdPatchComponent :: (MonadReader EntityID m, MonadIO m, MonadState ECS m) => m ()
removePdPatchComponent = do
    pd <- getPd

    -- If this is a poly patch, don't close it
    -- as that is handled by the poly patch system
    withComponent_ myPdPatch $ \PatchWithFile{..} ->
        unless pwfIsPoly $ do
            closePatch pd pwfPatch

    removeComponent myPdPatch
    removeComponent myOpenALSource

withPdPatch :: (MonadReader EntityID m, MonadState ECS m) => (Patch -> m b) -> m (Maybe b)
withPdPatch action = ask >>= \eid -> withEntityPdPatch eid action


withEntityPdPatch :: (MonadState ECS m) => EntityID -> (Patch -> m b) -> m (Maybe b)
withEntityPdPatch entityID action =
    withEntityComponent entityID myPdPatch $ \patch -> do
        action (pwfPatch patch)



sendEntitySynth :: (MonadIO m, MonadState ECS m) => EntityID -> Receiver -> Message -> m ()
sendEntitySynth entityID receiver message =
    void . withEntityPdPatch entityID $ \patch ->
        sendToPdPatch patch receiver message

sendSynth :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => Receiver -> Message -> m ()
sendSynth receiver message =
    void . withPdPatch $ \patch ->
        sendToPdPatch patch receiver message



sendToPdPatch :: (MonadIO m, MonadState ECS m) => Patch -> Receiver -> Message -> m ()
sendToPdPatch patch receiver message = withSystem_ sysSynth $ \soundSystem ->
    send (soundSystem ^. sndPd) patch receiver message

readPdArray :: (MonadReader EntityID m, MonadIO m, MonadState ECS m)
            => Receiver -> Int -> Int -> m (Vector Float)
readPdArray arrayName offset count = do
    pd <- viewSystem sysSynth sndPd
    fromMaybe V.empty . join <$> withPdPatch (\patch ->
        readArray pd (local patch arrayName) offset count)
