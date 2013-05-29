module Game.Hoggle.Board
       where

import Control.Applicative
import Control.Monad
import Data.Array (Array, elems, array)
import qualified Data.Array as Arr
import Data.List
import Data.Maybe

import Test.QuickCheck

data Board = Board {
  size   :: Int,
  spaces :: Array Int (Array Int Char)
  } deriving Eq

newtype Index = Ind (Int, Int) deriving (Show, Eq)

mkBoard :: Int -> [[Char]] -> Maybe Board
mkBoard n ccs = do
  as <- mapM upTo ccs
  b  <- upTo as
  return $ Board n b
  where upTo cs = do guard $ length cs == n
                     return $ array (0,n-1) . zip [0..] $ cs


toList :: Board -> [[Char]]
toList = map elems . elems . spaces

(!) :: Board -> Index -> Char
b ! (Ind (i,j)) = spaces b Arr.! i Arr.! j

neighbors :: Board -> Index -> [Index]
neighbors b (Ind (i,j)) = Ind <$> [(i', j')
                    | i' <- [i-1..i+1], i' >= 0, i' < size b,
                      j' <- [j-1..j+1], j' >= 0, j' < size b,
                      (i', j') /= (i,j)
                     ]

instance Show Board where
   show = unlines . map (intersperse ' ') . toList

instance Arbitrary Board where
  arbitrary = sized board'
    where board' 0 = pure . toBoard 0 $ []
          board' n = oneof [board' (n-1),
                            toBoard n <$> vectorOf n (vectorOf n (elements ['a'..'z']))]
          toBoard n bs = fromJust . mkBoard n $ bs

instance Arbitrary Index where
  arbitrary = sized $ \n ->
    do i <- choose (0,n)
       j <- choose (0,n)
       return $ Ind (i,j)