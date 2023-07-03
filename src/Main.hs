module Main where

import Main.Utf8 qualified as Utf8
import Sims3.Install
import Sims3.Patches.Hardware
import System.Path (absDir)

-- |
-- Main entry point.
--
-- The `, run` script will invoke this function.
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  install <-
    Sims3.Install.find
      (absDir "/mnt/hdd/SteamLibrary/steamapps/common/The Sims 3")
      (absDir "/mnt/hdd/SteamLibrary/steamapps/compatdata/47890/pfx/drive_c/users/steamuser/My Documents/Electronic Arts/The Sims 3")
  Utf8.withUtf8 $ do
    case install of
      Just i -> do
        putTextLn . show $ i
        _ <- Sims3.Patches.Hardware.apply i
        isHardwareApplied <- Sims3.Patches.Hardware.applied i
        putTextLn . show $ isHardwareApplied
      Nothing ->
        putTextLn "Error"

    putTextLn "Hello 🌎"
