import List
import Signal
import Time
import Graphics.Input as Input

cubes =
  [ "gzntc"
  , "boeut"
  , "rceob"
  , "stpua"
  , "asdfk"
  ]

const : a -> (b -> a)
const a = \_ -> a

null : [a] -> Bool
null xs = xs == []

transpose : [[a]] -> [[a]]
transpose xss = if null xss
             then []
             else (map head xss) :: transpose (filter (not . null) . map tail <| xss)

rotate : [[a]] -> [[a]]
rotate = map reverse . transpose

(rawBut, clicks) = Input.button "rotate!"

letters : Signal [[Char]]
letters = foldp (\_ cs -> rotate cs) cubes (clicks)

mmap : (a -> b) -> [[a]]  -> [[b]]
mmap = map . map  

inBox : Element -> Element
inBox = container 20 20 middle

toString : Char -> String
toString c = [c]

toTile : Char -> Element
toTile = inBox . plainText . toString

board : [[Char]] -> [[Element]]
board = mmap toTile

assemble : [[Element]] -> Element
assemble = flow down . map (flow right)

layout : Signal Element
layout = assemble . board <~ letters

buttn : Element
buttn = size 50 30 rawBut

arrange : Element -> Element -> Element
arrange btn lay = container 100 (widthOf lay) midTop btn

main : Signal Element
main = above <~ layout ~ (arrange buttn <~ layout)
