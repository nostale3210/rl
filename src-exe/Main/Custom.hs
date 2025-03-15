module Main.Custom where

import Data.Text qualified as T
import System.Random.Stateful qualified as Rand

data CustomDice
  = CustomDice {amount :: Int, faces :: [T.Text]}

type CustomRoll = [T.Text]

parseCustom :: T.Text -> CustomDice
parseCustom dice =
  let res = T.split (== '.') dice
      amount = read (T.unpack (head res)) :: Int
   in case res of
        [_, y :: T.Text] -> CustomDice amount $ T.split (== ',') y
        _ -> CustomDice 0 [T.pack "0"]

rollCustom :: CustomDice -> IO CustomRoll
rollCustom dice =
  ( Rand.uniformListRM
      (amount dice)
      (1, length $ faces dice)
      Rand.globalStdGen ::
      IO [Int]
  )
    >>= \pos ->
      return $ extractCustomResults dice pos

extractCustomResults :: CustomDice -> [Int] -> CustomRoll
extractCustomResults dice pos =
  case pos of
    [] -> []
    x : _ -> (faces dice !! (x - 1)) : extractCustomResults dice (tail pos)
