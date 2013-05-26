module Game.Hoggle.Trie
       where

import Data.Map as Map
import Data.ListTrie.Set as Set

type Dict = TrieSet Map Char

mkDict :: [String] -> Dict
mkDict = Set.fromList
