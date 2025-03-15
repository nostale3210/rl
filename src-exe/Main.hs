module Main where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.Text qualified as T
import Main.Cli qualified as Cli
import Main.Custom qualified as Custom
import Main.Default qualified as Default
import Main.Maths qualified as Maths
import Options.Applicative

main :: IO ()
main = do
  initRoll =<< execParser Cli.optsParser

initRoll :: Cli.Command -> IO ()
initRoll cmd =
  forM_
    (Cli.cmd cmd)
    ( \x ->
        if 'd' `elem` x
          then initDefaultRoll (T.pack x) cmd
          else initCustomRoll (T.pack x)
    )

initDefaultRoll :: T.Text -> Cli.Command -> IO ()
initDefaultRoll singleDie cmd = do
  -- Initial roll
  let defaultDice = Default.parseDefault singleDie
  putStrLn $ "Rolling " <> show singleDie <> "..."
  roll <- Default.rollDefault defaultDice
  putStr ">" >> mapM_ (\x -> putStr $ " " <> show x) roll >> putStrLn ""

  -- Discard lowest
  roll' <-
    if Cli.keepHOption cmd /= 0
      then do
        putStrLn $ "Discarding all dice expect " <> show (Cli.keepHOption cmd) <> " highest..."
        let tmpRoll = Maths.keep True (Cli.keepHOption cmd) roll
        putStr ">" >> mapM_ (\x -> putStr $ " " <> show x) tmpRoll >> putStrLn ""
        return tmpRoll
      else return roll

  -- Discard highest
  roll'' <-
    if Cli.keepLOption cmd /= 0
      then do
        putStrLn $ "Discarding all dice expect " <> show (Cli.keepLOption cmd) <> " lowest..."
        let tmpRoll = Maths.keep False (Cli.keepLOption cmd) roll'
        putStr ">" >> mapM_ (\x -> putStr $ " " <> show x) tmpRoll >> putStrLn ""
        return tmpRoll
      else return roll'

  -- Increase dice
  roll''' <-
    if Cli.increaseOption cmd /= 0
      then do
        putStrLn $ "Increasing dice by " <> show (Cli.increaseOption cmd) <> "..."
        let tmpRoll = Maths.increaseDefault (Cli.increaseOption cmd) roll''
        putStr ">" >> mapM_ (\x -> putStr $ " " <> show x) tmpRoll >> putStrLn ""
        return tmpRoll
      else return roll''

  -- Sum dice
  when (Cli.sumFlag cmd) $
    let rollSum = Maths.sumDefault roll''' in putStrLn $ "Summing roll...\n> " <> show rollSum

  -- Add to total
  when (Cli.addOption cmd /= 0) $
    let addSum = Maths.addToDefault (Cli.addOption cmd) roll'''
     in putStrLn $ "Adding " <> show (Cli.addOption cmd) <> "...\n> " <> show addSum

initCustomRoll :: T.Text -> IO ()
initCustomRoll singleDie = do
  let customDice = Custom.parseCustom singleDie
  putStrLn $ "Rolling " <> show singleDie <> "..."
  roll <- Custom.rollCustom customDice
  putStr ">" >> mapM_ (\x -> putStr $ " " <> show x) roll >> putStrLn ""
