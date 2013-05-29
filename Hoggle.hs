module Main
       where

import Game.Hoggle

import Control.Monad.State
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.List as List
import System.Environment
import System.Random

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> badCmd
    sub : flags -> dispatch sub flags

dispatch :: String -> [String] -> IO ()
dispatch cmd args = sub
  where sub = maybe badCmd ($ args) . List.lookup cmd $ subcommands

subcommands :: [(String, [String] -> IO ())]
subcommands = [("play", play), ("load", load)]

play :: [String] -> IO ()
play _ = getStdGen >>= putStr . show . evalState randMidBoard

load :: [String] -> IO ()
load _ = do c <- getContents
            let dict = mkDict . filter (isValid 3 17) . words $ c
            B.putStr . encode $ dict


badCmd :: IO ()
badCmd = putStrLn "Expected subcommand: [play, load]"
