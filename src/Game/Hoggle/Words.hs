module Game.Hoggle.Words
       where

isValid :: Int -> Int -> String -> Bool
isValid min max s = (length s >= min) && (length s < max) && notProper
  where notProper = case s of
          "" -> True
          _  -> head s `elem` ['a'..'z']
