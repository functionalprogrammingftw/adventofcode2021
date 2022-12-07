module Main where

import qualified MyLib (countFileSingleIncrements, countFileMultiIncrements)

main :: IO ()
main = do
  MyLib.countFileSingleIncrements
  MyLib.countFileMultiIncrements
