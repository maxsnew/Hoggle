module Game.Hoggle.Words
       where

isValid :: Int -> Int -> String -> Bool
isValid mn mx s = (length s >= mn) && (length s < mx) && notProper
  where notProper = case s of
          "" -> True
          _  -> head s `elem` ['a'..'z']
