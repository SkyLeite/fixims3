module Sims3.Install (Install, Sims3.Install.find, graphicsRules, Sims3.Install.readFile, Sims3.Install.writeFile, Sims3.Install.overwriteFile) where

import Data.Generics.Product
import Sims3.Modding as Modding
import System.Path as Path (AbsDir, AbsFile, RelFile, relDir, relFile, takeDirectory, takeFileName, (</>))
import System.Path.Directory (doesFileExist)
import System.Path.Directory qualified as Path
import System.Path.IO as Path.IO (hClose, hGetContents, hPutStr, openFile, openTempFile, writeFile)

data Install = Install
  { path :: Path.AbsDir,
    moddingStatus :: ModdingStatus
  }
  deriving (Generic, Show)

newtype ModdingStatus = Enabled Modding.Info | Disabled

-- Internal

isSims3Install :: Path.AbsDir -> IO Bool
isSims3Install installPath = do
  foundFiles <- mapM (\filePath -> doesFileExist (installPath </> filePath)) requiredFiles
  pure $ length foundFiles == length requiredFiles
  where
    requiredFiles :: [Path.RelFile]
    requiredFiles = [relFile "Game/Bin/Sims3Launcher.exe"]

-- Public

find :: Path.AbsDir -> IO (Maybe Install)
find installPath = do
  isInstall <- isSims3Install installPath

  if isInstall
    then
      pure $
        Just
          Install
            { path = installPath,
              isModEnabled = False
            }
    else pure Nothing

getInstallFilePath :: Install -> Path.RelFile -> Path.AbsFile
getInstallFilePath install path =
  getField @"path" install </> path

readFile :: Install -> Path.RelFile -> IO [Text]
readFile install path = do
  handle <- Path.IO.openFile (getInstallFilePath install path) ReadMode
  contents <- Path.IO.hGetContents handle
  evaluateWHNF (rnf contents)
  Path.IO.hClose handle
  return . lines . toText $ contents

writeFile :: Install -> Path.RelFile -> [Text] -> IO ()
writeFile install path =
  Path.IO.writeFile (getInstallFilePath install path) . toString . unlines

overwriteFile :: Install -> Path.RelFile -> ([Text] -> [Text]) -> IO ()
overwriteFile install path fn = do
  oldContent <- Sims3.Install.readFile install path
  (tempFile, handle) <- Path.IO.openTempFile (Path.takeDirectory destination) (Path.takeFileName destination)
  hPutStr handle . toString . unlines . fn $ oldContent
  hClose handle
  Path.removeFile destination
  Path.renameFile tempFile destination
  where
    destination = getInstallFilePath install path

graphicsRules :: Path.RelFile
graphicsRules =
  relDir "Game"
    </> relDir "Bin"
    </> relFile "GraphicsRules.sgr"
