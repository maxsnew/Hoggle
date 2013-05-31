module Game.Hoggle.Check
       (answers)
       where

import Game.Hoggle.Board
import Game.Hoggle.Trie as T

import Control.Applicative
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State

type SLR s r = StateT s (ListT (ReaderT r Identity))
type Loc = SLR (Dict, Index, [Index]) Board

answers :: Board -> Dict -> Dict
answers b d = mkDict $ do start <- Ind <$> [(i,j) | i <- inds, j <- inds]
                          (runLoc b d start startingAt)
  where inds = [0.. size b - 1]

runLoc :: Board -> Dict -> Index -> Loc a -> [a]
runLoc b d start l = runIdentity $ runReaderT (runListT (evalStateT l (d, start, []))) b

liftLoc :: [a] -> Loc a
liftLoc = lift . ListT . lift . return

getDict :: Loc Dict
getDict = do
  (d, _, _) <- get
  return d

curPref :: Loc String
curPref = do
  b              <- ask
  (_, cur, _)    <- get
  return $ case b ! cur of
    'q' -> "qu"
    c   -> [c]

nextLoc :: Loc ()
nextLoc = do
  b              <- ask
  (d, cur, prev) <- get
  next           <- liftLoc $ neighbors b cur
  guard (next `notElem` prev)
  pref           <- curPref
  let d' = withPrefix pref d
  guard (not . T.null $ d')
  put (d', next, cur:prev)

startingAt :: Loc String
startingAt = do
  d <- getDict
  pref <- curPref
  let s = if "" `T.member` d
          then return ""
          else mzero
  s `mplus` 
    (nextLoc >> (pref ++) <$> startingAt)
