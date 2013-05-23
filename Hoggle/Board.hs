module Hoggle.Board
       where

import Control.Applicative
import Control.Monad
import Data.Array (Array, elems, array)
import qualified Data.Array as Arr

data Board = Board {
  size   :: Int,
  spaces :: (Array Int (Array Int Char))
  }

mkBoard :: Int -> [[Char]] -> Maybe Board
mkBoard n ccs = do
  as <- mapM (upTo n) ccs
  b  <- upTo n as
  return $ Board n b
  where upTo n cs = do guard $ (length cs) == n
                       return $ array (0,n-1) . (zip [0..]) $ cs


toList :: Board -> [[Char]]
toList = map elems . elems . spaces

(!) :: Board -> (Int, Int) -> Char
b ! (i,j) = spaces b Arr.! i Arr.! j

neighbors :: Board -> (Int, Int) -> [(Int,Int)]
neighbors b (i,j) = [(i', j')
                    | i' <- [i-1..i+1], i' >= 0, i' < size b,
                      j' <- [j-1..j+1], j' >= 0, j' < size b,
                      (i', j') /= (i,j)
                     ]

instance Show Board where
   show = ("Board " ++) .  show . toList
