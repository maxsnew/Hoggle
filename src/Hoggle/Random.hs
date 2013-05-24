module Hoggle.Random
       where

import Hoggle.Board

import Control.Applicative
import Control.Monad.State
import Data.List as List
import Data.Maybe
import Data.Tuple
import System.Random

type Cube = String

cubes :: [Cube]
cubes =
  [ "aaafrs"
  , "aaeeee"
  , "aafirs"
  , "adennn"
  , "aeeeem"
  , "aeegmu"
  , "aegmnn"
  , "afirsy"
  , "bjkqxz"
  , "ccenst"
  , "ceiilt"
  , "ceilpt"
  , "ceipst"
  , "ddhnot"
  , "dhhlor"
  , "dhlnor"
  , "dhlnor"
  , "eiiitt"
  , "emottt"
  , "ensssu"
  , "fiprsy"
  , "gorrvw"
  , "iprrry"
  , "nootuw"
  , "ooottu"
  ]

randBoard :: (RandomGen g) => State g Board
randBoard = (fromJust . mkBoard size . splitUp size) <$> randL
  where n = length cubes
        size = floor . sqrt . fromIntegral $ n
        randL = shuffle cubes >>= mapM pick

splitUp :: Int -> [a] -> [[a]]
splitUp 0 xs = [xs]
splitUp n xs | n > 0 = case end of
  []        -> [beg]
  otherwise -> beg : splitUp n end
  where (beg, end) = splitAt n xs

shuffle :: (Eq a, RandomGen g) => [a] -> State g [a]
shuffle [] = return []
shuffle xs = do
  x'  <- pick xs
  xs' <- shuffle $ List.delete x' xs
  return $ x':xs'

getRand :: (RandomGen g) => State g Int
getRand = state next

randIndex n = liftM (`mod` n) getRand

pick xs = liftM (xs !!) (randIndex . length $ xs)
