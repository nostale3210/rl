module Main.Maths where

import Data.List (sort, sortBy)
import Data.Ord (Down (Down), comparing)
import Main.Default qualified as Default

sumDefault :: Default.DefaultRoll -> Int
sumDefault = sum

increaseDefault :: Int -> Default.DefaultRoll -> Default.DefaultRoll
increaseDefault inc = map (+ inc)

addToDefault :: Int -> Default.DefaultRoll -> Int
addToDefault inc = (+) inc . sumDefault

expectation :: Int -> Float -> Float
expectation faces probability =
  let ffaces = read (show faces) :: Float
   in case ffaces of
        0 -> 0
        x -> x * probability + expectation (faces - 1) probability

median :: Default.DefaultDice -> Float
median roll =
  (read (show $ Default.amount roll) :: Float)
    * expectation
      (Default.faces roll)
      (1 / (read (show $ Default.faces roll) :: Float))

keep :: Bool -> Int -> Default.DefaultRoll -> Default.DefaultRoll
keep highest amount roll =
  if highest
    then take amount $ sortBy (comparing Down) roll
    else take amount $ sort roll
