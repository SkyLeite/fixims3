module Sims3.Types where

import System.Path qualified as Path

data Install = Install
  { path :: Path.AbsDir,
    moddingStatus :: ModdingStatus,
    dataPath :: Path.AbsDir
  }
  deriving (Generic, Show)

data Mod = Mod {} deriving (Generic, Show)

data ModdingInfo = ModdingInfo
  {mods :: [Mod]}
  deriving (Generic, Show)

data ModdingStatus
  = Enabled ModdingInfo
  | Disabled
  deriving (Generic, Show)
