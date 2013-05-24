module Main
       where

import Hoggle.Random

import Control.Monad.State
import System.Random

main :: IO ()
main = getStdGen >>= print . evalState randBoard 