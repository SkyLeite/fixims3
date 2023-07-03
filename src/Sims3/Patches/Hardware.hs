module Sims3.Patches.Hardware where

import Data.List
import Sims3 qualified
import Sims3.IO qualified
import Sims3.Install as Install (graphicsRules)

configLines :: [Text]
configLines = ["seti forcedCardLevel $cardLevelUber", "seti forcedCpuLevel $cpuLevelUber"]

applied :: Sims3.Install -> IO Bool
applied install =
  isSubsequenceOf configLines <$> Sims3.IO.readInstallFile install Install.graphicsRules

apply :: Sims3.Install -> IO ()
apply install = do
  isApplied <- applied install
  unless
    isApplied
    (Sims3.IO.overwriteInstallFile install Install.graphicsRules appendRules)
  where
    appendRules :: [Text] -> [Text]
    appendRules = (++) configLines
