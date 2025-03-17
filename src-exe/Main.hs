module Main where

import Control.Monad (when)
import Data.Text qualified as T
import Data.Text.IO qualified as Ti
import Main.Cli qualified as Cli
import Main.Custom qualified as Custom
import Main.Default qualified as Default
import Main.Maths qualified as Maths
import Main.Util qualified as Util
import Options.Applicative

main :: IO ()
main = initRoll =<< execParser Cli.optsParser

initRoll :: Cli.Command -> IO ()
initRoll cmd =
  mapM_
    ( \x ->
        if '.' `T.elem` x
          then initCustomRoll x
          else initDefaultRoll x cmd
    )
    (Cli.cmd cmd)

initDefaultRoll :: T.Text -> Cli.Command -> IO ()
initDefaultRoll singleDie cmd = do
  let defaultDice = Default.parseDefault singleDie

  -- Calculate median
  when (Cli.medianFlag cmd) $
    let median = Maths.median defaultDice
     in Util.printStrList ["Calculating median...\n>  ", show median]

  -- Initial roll
  Util.printStrList ["Rolling ", show singleDie, "..."]
  roll <- Default.rollDefault defaultDice
  Util.printRes roll

  -- Discard lowest
  roll' <-
    if Cli.keepHOption cmd /= 0
      then
        Util.printStrList ["Discarding all dice expect ", show $ Cli.keepHOption cmd, " highest..."]
          >> let tmpRoll = Maths.keep True (Cli.keepHOption cmd) roll
              in Util.printRes tmpRoll >> return tmpRoll
      else return roll

  -- Discard highest
  roll'' <-
    if Cli.keepLOption cmd /= 0
      then
        Util.printStrList ["Discarding all dice expect ", show (Cli.keepLOption cmd), " lowest..."]
          >> let tmpRoll = Maths.keep False (Cli.keepLOption cmd) roll'
              in Util.printRes tmpRoll >> return tmpRoll
      else return roll'

  -- Increase dice
  roll''' <-
    if Cli.increaseOption cmd /= 0
      then
        Util.printStrList ["Increasing dice by ", show (Cli.increaseOption cmd), "..."]
          >> let tmpRoll = Maths.increaseDefault (Cli.increaseOption cmd) roll''
              in Util.printRes tmpRoll >> return tmpRoll
      else return roll''

  -- Sum dice
  when (Cli.sumFlag cmd) $
    let rollSum = Maths.sumDefault roll''' in Util.printStrList ["Summing roll...\n>  ", show rollSum]

  -- Add to total
  when (Cli.addOption cmd /= 0) $
    let addSum = Maths.addToDefault (Cli.addOption cmd) roll'''
     in Util.printStrList ["Adding ", show (Cli.addOption cmd), "...\n>  ", show addSum]

initCustomRoll :: T.Text -> IO ()
initCustomRoll singleDie = do
  let customDice = Custom.parseCustom singleDie
  Ti.putStrLn $ T.concat [T.pack "Rolling \"", singleDie, T.pack "\"..."]
  roll <- Custom.rollCustom customDice
  Util.printResText roll
