module Main
       where

import Game.Hoggle.Board

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

main = defaultMain tests

tests = [
  testGroup "Board Tests" [
     testProperty "Good Reference" prop_good_reference
                          ]
  , testGroup "Neighbor Tests" [
     testProperty "adjacent neighbors" prop_adjacent_neighbors
     , testProperty "neighbors on board" prop_on_board_neighbors
     ]
  ]

isInRange :: Index -> Board -> Bool
isInRange (Ind (i,j)) b = not ((len <= 0) || (i >= len) || (j >= len))
  where len = size b
                 

prop_good_reference :: Index -> Board -> Property
prop_good_reference ind@(Ind (i,j)) b = property $
                                        ind `isInRange` b
                                        ==> (b ! ind) == ((toList b) !! i) !! j

prop_adjacent_neighbors :: Index -> Board -> Property
prop_adjacent_neighbors ind@(Ind (i,j)) b = property $
                                            ind `isInRange` b
                                            ==> all offBy1 . neighbors b $ ind
  where offBy1 (Ind (i',j')) = (dist i i') `elem` [0,1]
                               && (dist i i') `elem` [0,1]
                               && not ((i == i') && (j == j'))
        dist x y = abs (x - y)

prop_on_board_neighbors :: Index -> Board -> Property
prop_on_board_neighbors ind b = property $
                                ind `isInRange` b
                                ==> all (`isInRange` b) . neighbors b $ ind