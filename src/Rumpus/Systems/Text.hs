{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Rumpus.Systems.Text where
import PreludeExtra hiding (Key)
import Graphics.GL.Freetype
import Graphics.GL.TextBuffer
data TextSystem = TextSystem
        { _txtFont :: Font
        , _txtRecycleBin :: [TextRenderer]
        }
makeLenses ''TextSystem

defineSystemKey ''TextSystem

defineComponentKey ''TextRenderer
defineComponentKeyWithType "Text"      [t|String|]
defineComponentKeyWithType "TextPose"  [t|M44 GLfloat|]
defineComponentKeyWithType "TextCachedM44"  [t|M44 GLfloat|]
defineComponentKeyWithType "TextColor" [t|V4 GLfloat|]

getFont :: (MonadState ECS m) => m Font
getFont = viewSystem sysText txtFont

initTextSystem :: (MonadIO m, MonadState ECS m) => m ()
initTextSystem = do
    glyphProg <- createShaderProgram "resources/shaders/glyph.vert" "resources/shaders/glyph.frag"
    font      <- createFont "resources/fonts/SourceCodePro-Regular.ttf" 50 glyphProg

    registerSystem sysText $ TextSystem
        { _txtFont       = font
        , _txtRecycleBin = []
        }

    registerComponent "Text"          myText           (savedComponentInterface myText)
    registerComponent "TextPose"      myTextPose       (savedComponentInterface myTextPose)
    registerComponent "TextCachedM44" myTextCachedM44  (newComponentInterface myTextCachedM44)
    registerComponent "TextColor"     myTextColor      (savedComponentInterface myTextColor)
    registerComponent "TextRenderer"  myTextRenderer $ (newComponentInterface myTextRenderer)
        { ciDeriveComponent = Just $ withComponent_ myText $ \text -> do
            textRenderer <- modifySystemState sysText $ do
                renderers <- use txtRecycleBin
                case renderers of
                    [] -> createTextRenderer font (textBufferFromString text)
                    (x:xs) -> do
                        txtRecycleBin .= xs
                        flip execStateT x $ setTextRendererText id text
            myTextRenderer ==> textRenderer
            -- Retrigger pose caching
            setTextPose =<< getTextPose
        , ciRemoveComponent = withComponent_ myTextRenderer $ \textRenderer -> do
            -- FIXME at some point we should start actually destroying TextRenderers if the bin has enough
            clearedTextRenderer <- flip execStateT textRenderer $ setTextRendererText id ""
            modifySystemState sysText $ txtRecycleBin %= (clearedTextRenderer:)
            removeComponent myTextRenderer
        }

setEntityText :: (MonadIO m, MonadState ECS m) => EntityID -> String -> m ()
setEntityText entityID text = do
    setEntityComponent myText text entityID
    modifyEntityComponent entityID myTextRenderer $ \renderer -> 
        flip execStateT renderer $ setTextRendererText id text
    -- Recache scale matrix
    setEntityTextPose entityID =<< getEntityTextPose entityID

setText :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => String -> m ()
setText text = flip setEntityText text =<< ask

getEntityTextColor :: MonadState ECS m => EntityID -> m (V4 GLfloat)
getEntityTextColor entityID = fromMaybe 1 <$> getEntityComponent entityID myTextColor

getTextColor :: (MonadReader EntityID m, MonadState ECS m) => m (V4 GLfloat)
getTextColor = getEntityTextColor =<< ask


getEntityTextPose :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getEntityTextPose entityID = fromMaybe identity <$> getEntityComponent entityID myTextPose

getEntityTextCachedM44 :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getEntityTextCachedM44 entityID = fromMaybe identity <$> getEntityComponent entityID myTextCachedM44

setTextPose :: (MonadState ECS m, MonadReader EntityID m) => M44 GLfloat -> m ()
setTextPose pose = flip setEntityTextPose pose =<< ask

setEntityTextPose :: (MonadState ECS m) => EntityID -> M44 GLfloat -> m ()
setEntityTextPose entityID textPose = do
    setEntityComponent myTextPose textPose entityID

    withEntityComponent_ entityID myTextRenderer $ \textRenderer -> 
        setEntityComponent myTextCachedM44 (textPose !*! textRenderer ^. txrCorrectionM44) entityID


getTextPose :: (MonadReader EntityID m, MonadState ECS m) => m (M44 GLfloat)
getTextPose = getEntityTextPose =<< ask