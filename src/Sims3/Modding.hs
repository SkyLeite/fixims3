module Sims3.Modding where

import System.Path as Path (AbsDir, RelDir, RelFile, relDir, (</>))

data Info = Info
  {mods :: [Mod]}

data Mod = Mod
  { name :: Text,
    description :: Text,
    files :: [Path.RelFile]
  }

packagesDir :: Path.RelDir
packagesDir = relDir "Mods" </> relDir "Fixims3"

mkInfo :: Path.AbsDir -> IO (Maybe Info)
mkInfo dataDir = do
  pure Nothing
