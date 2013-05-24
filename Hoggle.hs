module Main
       where

import Game.Hoggle

import Control.Monad.State
import System.Random

main :: IO ()
main = getStdGen >>= print . evalState randBoard 