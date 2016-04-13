{-# LANGUAGE FlexibleContexts #-}

module Rumpus.CopyScenes where
import Rumpus.Types
import PreludeExtra

-- | Copy the 'pristine' Scenes folder into the user's Documents/Rumpus directory on startup
-- if the Documents/Rumpus folder is missing.  
copyStartScene :: FilePath -> IO FilePath
copyStartScene sceneFolderName = do

    userDocsDir <- getUserDocumentsDirectory
    let userMainSceneDir  = userDocsDir </> "Rumpus" </> "Scenes" </> sceneFolderName
        pristineSceneDir  = pristineSceneDirWithName sceneFolderName
    
    exists  <- doesDirectoryExist userMainSceneDir
    when (not exists) $ do
        createDirectoryIfMissing True userMainSceneDir
        roomFiles <- filter (not . (`elem` [".", ".."])) <$> getDirectoryContents pristineSceneDir

        forM_ roomFiles $ \roomFile -> 
            copyFile (pristineSceneDir </> roomFile) (userMainSceneDir </> roomFile)
    
    return userMainSceneDir

pristineSceneDirWithName name = "pristine"               </> "Scenes" </> name