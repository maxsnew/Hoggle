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

on : (a -> a -> b) -> (c -> a) -> c -> c -> b
on f chng x y = f (chng x) (chng y)

const : a -> (b -> a)
const a = \_ -> a

null : [a] -> Bool
null xs = xs == []

transpose : [[a]] -> [[a]]
transpose xss = if null xss
             then []
             else (map head xss) :: transpose (filter (not . null) . map tail <| xss)

data Dir = Clock | CtrClock

rotate : Dir -> [[a]] -> [[a]]
rotate d = if | d == Clock    -> map reverse . transpose
              | d == CtrClock -> transpose . map reverse

(clockBut, clockClicks) = Input.button "clockwise"
(ctrBut, ctrClicks) = Input.button "counter-clockwise"

clocks = sampleOn clockClicks (constant Clock)
ctrs = sampleOn ctrClicks (constant CtrClock)

letters : Signal [[Char]]
letters = foldp (\c brd -> rotate c brd) cubes (merge clocks ctrs)

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

sizedBut : Element -> Element
sizedBut btn = size 75 30 btn

arrange : Element -> Element -> Element
arrange btn lay = container ((max `on` widthOf) lay btn) 35 midTop . sizedBut <| btn

main : Signal Element
main = flow down <~ combine [layout,
                             arrange clockBut <~ layout,
                             arrange ctrBut <~ layout]
