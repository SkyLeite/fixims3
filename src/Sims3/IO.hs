module Sims3.IO (readInstallFile, overwriteInstallFile, readDataFile, overwriteDataFile) where

import Data.Generics.Product (getField)
import Sims3.Types
import System.Path as Path (AbsDir, RelFile, takeDirectory, takeFileName, (</>))
import System.Path.Directory qualified as Path.Directory
import System.Path.IO as Path.IO
  ( hClose,
    hGetContents,
    hPutStr,
    openFile,
    openTempFile,
  )
import Prelude hiding (readFile)

-- Public

--- Install

readInstallFile :: Sims3.Types.Install -> Path.RelFile -> IO [Text]
readInstallFile install = readFile $ getField @"path" install

overwriteInstallFile :: Sims3.Types.Install -> Path.RelFile -> ([Text] -> [Text]) -> IO ()
overwriteInstallFile install = overwriteFile $ getField @"path" install

--- Data

readDataFile :: Sims3.Types.Install -> Path.RelFile -> IO [Text]
readDataFile install = readFile $ getField @"dataPath" install

overwriteDataFile :: Sims3.Types.Install -> Path.RelFile -> ([Text] -> [Text]) -> IO ()
overwriteDataFile install = overwriteFile $ getField @"dataPath" install

-- Internal

readFile :: Path.AbsDir -> Path.RelFile -> IO [Text]
readFile baseDir path = do
  handle <- Path.IO.openFile (baseDir </> path) ReadMode
  contents <- Path.IO.hGetContents handle
  evaluateWHNF (rnf contents)
  Path.IO.hClose handle
  return . lines . toText $ contents

overwriteFile :: Path.AbsDir -> Path.RelFile -> ([Text] -> [Text]) -> IO ()
overwriteFile baseDir path fn = do
  oldContent <- readFile baseDir path
  (tempFile, handle) <- Path.IO.openTempFile (Path.takeDirectory destination) (Path.takeFileName destination)
  Path.IO.hPutStr handle . toString . unlines . fn $ oldContent
  Path.IO.hClose handle
  Path.Directory.removeFile destination
  Path.Directory.renameFile tempFile destination
  where
    destination = baseDir </> path
