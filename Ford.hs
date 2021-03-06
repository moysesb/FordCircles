{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where
import Data.Ratio
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

coprimes :: (Integral a) => a -> [a]
coprimes n
	| n <= 0 = []
	| otherwise = [x | x <- [0..n], gcd n x == 1]

fordRatio :: Integer -> Ratio Integer
fordRatio q = 1 % ( 2 * (q ^ 2) )

fordCircle :: Integer -> Integer-> (P2, Diagram B R2)
fordCircle p q = (p2 (x, y), circle radius # lw ultraThin) 
			where radius = fromRational $ fordRatio q
			      x      = fromIntegral p / fromIntegral q
			      y      = radius

fordCircles' :: Integer -> [(P2, Diagram B R2)]
fordCircles' depth = [fordCircle p q | q <- [1..depth], p <- (coprimes q)]  

fordCircles = (position $ fordCircles' depth)
yAxis = (position [(p2 (0, 0.5), vrule 1)]) # lw veryThin
xAxis = (position [(p2 (0.5, 0), hrule 1)]) # lw veryThin

depth = 16
main :: IO ()
main = defaultMain $ yAxis `atop` fordCircles === xAxis
