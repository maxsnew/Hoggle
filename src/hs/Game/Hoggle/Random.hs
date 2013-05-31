module Game.Hoggle.Random
       ( randBoard
       , fourCubes
       , fiveCubes
       , randMidBoard
       , randBigBoard
       )
       where

import Game.Hoggle.Board

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.List as List
import Data.Maybe
import System.Random

type Cube = String

fourCubes :: [Cube]
fourCubes =
  [ "aaciot"
  , "ahmors"
  , "egkluy"
  , "abilty"
  , "acdemp"
  , "egintv"
  , "gilruw"
  , "elpstu"
  , "denosw"
  , "acelrs"
  , "abjmoq"
  , "eefhiy"
  , "ehinps"
  , "dknotu"
  , "adenvz"
  , "biforx"
  ]

fiveCubes :: [Cube]
fiveCubes =
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

type MR = MaybeT Rand
type Rand = StateT StdGen Identity
randMidBoard :: Rand Board
randMidBoard = fromJust <$> (runMaybeT $ randBoard fourCubes)

randBigBoard :: Rand Board
randBigBoard = fromJust <$> (runMaybeT $ randBoard fiveCubes)

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

randBoard :: [Cube] -> MR Board
randBoard cubes = do l <- lift randL
                     let letters = splitUp n l
                     liftMaybe . mkBoard n $ letters
  where n     = floor . sqrt . fromIntegral . length $ cubes
        randL = shuffle cubes >>= mapM pick

splitUp :: Int -> [a] -> [[a]]
splitUp 0 xs = [xs]
splitUp n xs | n > 0 = case end of
  []        -> [beg]
  _         -> beg : splitUp n end
  where (beg, end) = splitAt n xs
splitUp _ _ = error "splitUp takes Non-negative arguments"                     

shuffle :: (Eq a) => [a] -> Rand [a]
shuffle [] = return []
shuffle xs = do
  x'  <- pick xs
  xs' <- shuffle $ List.delete x' xs
  return $ x':xs'

getRand :: Rand Int
getRand = state next

randIndex :: Int -> Rand Int
randIndex n = liftM (`mod` n) getRand

pick :: [a] -> Rand a
pick xs = liftM (xs !!) (randIndex . length $ xs)
