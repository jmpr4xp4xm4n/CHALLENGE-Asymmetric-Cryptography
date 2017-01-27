module Main where

import Control.Arrow
import Data.List
import Math.NumberTheory.Moduli

cp = 2^1024 - 1093337
cg = 7
cy = 61866201876285640738560054986039646277183235081464596666803083888384286111662629​82406518313438601892255572681999822014453307246463068376384156176082291268395302​92767965862079731217551387118069961755120098787914485625672341107972067385487537​14646123243712070687176172027733573067386801703238975473191649230385

addExp :: Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
addExp n f =
    concatMap $
  takeWhile ((< n) . fst) .
  iterate ((*f) *** (\x -> powerModInteger x f n))

main :: IO ()
main =
    print $
  find ((== cy) . snd) .
  addExp cp 5 .
  addExp cp 3 .
  addExp cp 2 $
  [(1, cg)]
