{-# LANGUAGE FlexibleContexts #-}

module Rumpus.CopyScenes where
import Rumpus.Types
import PreludeExtra

-- | Copy the 'pristine' Scenes folder into the user's Documents/Rumpus directory on startup
-- if the Documents/Rumpus folder is missing.  
copyScenes :: IO FilePath
copyScenes = do
    userDocsDir <- getUserDocumentsDirectory
    let userRoomDir    = userDocsDir </> "Rumpus" </> "Scenes" </> "Room"
        pristineScenes = "pristine"               </> "Scenes" </> "Room"
    exists  <- doesDirectoryExist userRoomDir
    when (not exists) $ do
        createDirectoryIfMissing True userRoomDir
        roomFiles <- filter (not . (`elem` [".", ".."])) <$> getDirectoryContents pristineScenes

        forM_ roomFiles $ \roomFile -> 
            copyFile (pristineScenes </> roomFile) (userRoomDir </> roomFile)
    -- When not in release mode, we want to edit the pristine folder directly
    -- so we can track changes in git.
    return $ if isInReleaseMode then userRoomDir else pristineScenes