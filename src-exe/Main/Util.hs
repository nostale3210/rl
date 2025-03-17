module Main.Util where

import Data.Text qualified as T
import Data.Text.IO qualified as Ti

printStrList :: [String] -> IO ()
printStrList strs = Ti.putStrLn . T.concat $ map T.pack strs

printRes :: [Int] -> IO ()
printRes roll =
  (Ti.putStr . T.pack $ "> ")
    >> mapM_ (\x -> Ti.putStr (T.pack " " <> (T.pack . show $ x))) roll
    >> (Ti.putStr . T.pack $ "\n")

printResText :: [T.Text] -> IO ()
printResText roll =
  (Ti.putStr . T.pack $ "> ")
    >> mapM_ (\x -> Ti.putStr (T.pack " " <> x)) roll
    >> (Ti.putStr . T.pack $ "\n")
