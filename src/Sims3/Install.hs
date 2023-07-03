module Sims3.Install
  ( Install,
    Sims3.Install.find,
    graphicsRules,
  )
where

import Sims3.Modding as Modding
import Sims3.Types
import System.Path as Path (AbsDir, RelFile, relDir, relFile, takeDirName, (</>))
import System.Path.Directory (doesFileExist)
import System.Path.Directory qualified as Path

-- Internal

isSims3Install :: Path.AbsDir -> IO Bool
isSims3Install installPath = do
  foundFiles <- mapM (\filePath -> doesFileExist (installPath Path.</> filePath)) requiredFiles
  pure $ length foundFiles == length requiredFiles
  where
    requiredFiles :: [Path.RelFile]
    requiredFiles = [Path.relFile "Game/Bin/Sims3Launcher.exe"]

isDataDir :: Path.AbsDir -> IO Bool
isDataDir dataPath = do
  hasVersionFile <- Path.doesFileExist (dataPath Path.</> Path.relFile "Version.tag")
  return $ hasVersionFile && isNameCorrect
  where
    isNameCorrect = case Path.takeDirName dataPath of
      Just dirName -> dirName == Path.relDir "The Sims 3"
      Nothing -> False

-- Public

find :: Path.AbsDir -> Path.AbsDir -> IO (Maybe Install)
find installPath dataPath = do
  isInstall <- isSims3Install installPath
  isData <- isDataDir dataPath

  if isInstall && isData
    then do
      let initialInstall =
            Install
              { path = installPath,
                moddingStatus = Disabled,
                dataPath = dataPath
              }

      moddingStatus <- Modding.mkInfo initialInstall
      pure $ Just (initialInstall {moddingStatus = moddingStatus})
    else pure Nothing

graphicsRules :: Path.RelFile
graphicsRules =
  Path.relDir "Game"
    Path.</> Path.relDir "Bin"
    Path.</> Path.relFile "GraphicsRules.sgr"
