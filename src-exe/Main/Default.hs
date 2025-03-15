module Main.Default where

import Data.Text qualified as T
import System.Random.Stateful qualified as Rand

data DefaultDice
  = DefaultDice {amount :: Int, faces :: Int}
  deriving (Show)

type DefaultRoll = [Int]

parseDefault :: T.Text -> DefaultDice
parseDefault dice =
  let res = map (\x -> read (T.unpack x) :: Int) $ T.split (== 'd') dice
   in case res of
        [x :: Int, y :: Int] -> DefaultDice x y
        _ -> DefaultDice 0 0

rollDefault :: DefaultDice -> IO DefaultRoll
rollDefault dice =
  Rand.uniformListRM (amount dice) (1, faces dice) Rand.globalStdGen :: IO [Int]
