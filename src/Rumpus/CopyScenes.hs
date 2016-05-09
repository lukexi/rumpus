{-# LANGUAGE FlexibleContexts #-}

module Rumpus.CopyScenes where
--import PreludeExtra hiding ((</>), FilePath)
import System.Directory (getUserDocumentsDirectory)
import Prelude hiding (FilePath)
import Shelly
import Filesystem.Path.CurrentOS hiding ((</>))
-- | Copy the 'pristine' Scenes folder into the user's Documents/Rumpus directory on startup
-- if the Documents/Rumpus folder is missing.

{-
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
-}

copyStartScene :: String -> IO String
copyStartScene sceneFolderName = do

    userDocsDir <- getUserDocumentsDirectory

    shelly $ do
        let userMainSceneDir  = userDocsDir </> "Rumpus" </> "Scenes"
            pristineSceneDir  = pristineSceneDirWithName_ sceneFolderName

        exists <- test_d userMainSceneDir
        when (not exists) $ do
            mkdir_p userMainSceneDir
            cp_r pristineSceneDir userMainSceneDir

        return (encodeString (userMainSceneDir </> sceneFolderName))

pristineSceneDirWithName_ :: String -> FilePath
pristineSceneDirWithName_ name = "pristine" </> "Scenes" </> name

pristineSceneDirWithName = encodeString . pristineSceneDirWithName_
