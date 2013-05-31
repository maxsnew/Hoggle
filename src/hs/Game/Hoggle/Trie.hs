module Game.Hoggle.Trie
       where

import Data.Map as Map
import Data.ListTrie.Set as Set

type Dict = TrieSet Map Char

mkDict :: [String] -> Dict
mkDict = Set.fromList

toList :: Dict -> [String]
toList = Set.toList

null :: Dict -> Bool
null = Set.null

withPrefix :: String -> Dict -> Dict
withPrefix = deletePrefix

member :: String -> Dict -> Bool
member = Set.member

delete :: String -> Dict -> Dict
delete = Set.delete