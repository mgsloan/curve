-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Bezier
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- 1D Bezier curve representation.

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-} 
module Data.Curve.Bezier where

import Data.Curve.Classes
import qualified Data.Curve.Interval as I
import Data.Curve.Linear
import Data.Curve.Util

import Control.Arrow
import qualified Data.Array as A
import Numeric.Rounding
import Data.VectorSpace

data Bezier a = Bezier { bezierCoefs :: [a] } deriving (Show)

bezierFromPoints :: [(a, a)] -> (Bezier a, Bezier a)
bezierFromPoints = mapT Bezier . (map fst &&& map snd)

order = subtract 1 . length . bezierCoefs

-- TODO: is this efficient?
deCasteljau b t = coefs
  where n = order b
        coefs = A.listArray ((0,0),(n,n)) $ map de' [(i,j) | i<-[0..n], j<-[0..n]]
        de' (i, 0) = bezierCoefs b !! i
        de' (i, j) = (((1-t)*) $ coefs A.! (i,   j-1)) +
                     ((t*)     $ coefs A.! (i+1, j-1))

splitBezier b t = (Bezier [de A.! (0, i)     | i <- [0..n-1]],
                   Bezier [de A.! (n - i, i) | i <- [0..n-1]])
  where de = deCasteljau b t
        (_, (_, n)) = A.bounds de

-- lift 1
bezmap :: (a -> a) -> Bezier a -> Bezier a
bezmap f (Bezier xs) = Bezier (map f xs)

{- old version:
coeffs = zipWith (*) (sunfoldr (*t) 1)
                     (1.0 : zipWith (\x y -> fromIntegral x / fromIntegral y) [n,n-1..0] [1..n])
-}

instance (Num a) => Curve (Bezier a) where
    type Domain (Bezier a) = a
    type Codomain (Bezier a) = a

    -- TODO: if we statically know the order, don't perform length
    -- TODO: memoize bezierCoefs / pascal?
    -- TODO: evaluation functions which retrieve derivative stack
    at (Bezier []) _ = 0
    at b 0 = head . bezierCoefs $ b
    at b 1 = last . bezierCoefs $ b
    at b t = (deCasteljau b t) A.! (0, n)
      where n = order b

instance (IsFinite a) => IsFinite (Bezier a) where
    isFinite = all isFinite . bezierCoefs

instance (Num a) => IsZero (Bezier a) where
    isZero = all (==0) . bezierCoefs

instance (Precision a) => FunctionBounds (Bezier a) where
    type DomainBounds (Bezier a) = I.Interval a
    type CodomainBounds (Bezier a) = I.Interval a
    domain = const $ I.unit
    --TODO more efficient local impl?
    bounds (I.I (Round 0) (Round 1)) = I.fromList . bezierCoefs
    bounds ivl = bounds I.unit . portion ivl

instance (Precision a) => Portionable (Bezier a) where
    portion = undefined

instance (Num a) => AdditiveGroup (Bezier a) where
    zeroV = (Bezier [])
    (^+^) = undefined
    negateV = bezmap negate

instance (Num a) => VectorSpace (Bezier a) where
    type Scalar (Bezier a) = a
    (*^) s = bezmap (s*)

instance (Fractional a) => Integrable (Bezier a) where
    derivative (Bezier [x, y]) = Bezier [y - x]
    derivative (Bezier xs) = Bezier
      . map ((fromIntegral $ length xs - 1)*)
      . zipWith (-) xs $ tail xs
    integral (Bezier xs) = Bezier . tail
      . scanl (\a -> (a+) . (/(fromIntegral $ length xs))) 0 $ xs

instance (Num a) => Offsetable (Bezier a) where
    offset x (Bezier []) = Bezier [x]
    offset x b = bezmap (+x) b
