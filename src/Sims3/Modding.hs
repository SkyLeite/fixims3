module Sims3.Modding where

import Data.Generics.Product (getField)
import Sims3.IO qualified
import Sims3.Types (ModdingInfo (ModdingInfo))
import Sims3.Types qualified
import System.Path as Path (RelDir, relDir, relFile, (</>))
import System.Path.Directory qualified as Path

packagesDir :: Path.RelDir
packagesDir = Path.relDir "Mods" Path.</> Path.relDir "Fixims3"

isResourceFilePatched :: [Text] -> Bool
isResourceFilePatched =
  elem "PackedFile Fixims3/Packages/*.package"

mkInfo :: Sims3.Types.Install -> IO Sims3.Types.ModdingStatus
mkInfo install = do
  isResourceFilePresent <- Path.doesFileExist (dataPath Path.</> Path.relDir "Mods" Path.</> Path.relFile "Resource.cfg")
  resourceFile <- Sims3.IO.readDataFile install (Path.relDir "Mods" Path.</> Path.relFile "Resource.cfg")

  pure $
    if isResourceFilePresent && isResourceFilePatched resourceFile
      then Sims3.Types.Enabled (ModdingInfo {mods = []})
      else Sims3.Types.Disabled
  where
    dataPath = getField @"dataPath" install
