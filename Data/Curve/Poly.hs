-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Poly
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Polynomial function representation.

{-# LANGUAGE TypeFamilies #-} 

module Data.Curve.Poly where

import Data.Curve.Classes
import Data.Curve.Function
import qualified Data.Curve.Interval as I
import Data.Curve.Linear
import Data.Curve.Math
import Data.Curve.Util

import Control.Arrow ((***), second)
import Numeric.Rounding

data Poly a = Poly { polyCoefs :: [a] } deriving (Show)

polymap f (Poly xs) = Poly (map f xs) 
polyzip f (Poly xs) (Poly ys) = Poly (zipWith f xs ys)

-- I'm tickled by the fact that "oddsevens" and "evensodds" are both ambiguous.
evensodds :: [a] -> ([a], [a])
evensodds [] = ([], [])
evensodds [x] = ([x], [])
evensodds (x:y:xs) = (x:) *** (y:) $ evensodds xs

unevensodds = concat . uncurry (zipWith (\x y -> [x, y]))

instance (Num a) => Curve (Poly a) where
    type Domain (Poly a) = a
    type Codomain (Poly a) = a

    at (Poly xs) 0 = head xs
    at (Poly xs) t = sum $ zipWith (*) (sunfoldr (*t) 1) xs

instance (IsFinite a) => IsFinite (Poly a) where
    isFinite = all isFinite . polyCoefs

instance (IsZero a) => IsZero (Poly a) where
    isZero = all isZero . polyCoefs

instance (Precision a) => FunctionBounds (Poly a) where
    type DomainBounds (Poly a) = I.Interval a
    type CodomainBounds (Poly a) = I.Interval a
    domain _ = I.unit
    bounds = extremaBounds

--instance (Precision a) => Portionable (Poly a) where
--    portion = undefined

instance (Num a) => Offsetable (Poly a) where
    offset y (Poly []) = Poly [y]
    offset y (Poly (x:xs)) = Poly $ (x+y):xs

-- TODO: better implementation
instance (Precision a) => Rootable (Poly a) where
    rootsAt f = rootsAt $ at f

instance (RealFloat a) => Integrable (Poly a) where
    integral = undefined
    derivative = undefined

-- pad if n < 0, drop if n > 0
padrop n v xs | n < 0 = replicate (-n) v ++ xs
padrop n v xs = drop n xs

{-
derivative order = Poly $ padrop order 0 . polyCoefs
  where mults = map (\i -> product [i..i+order]) [1..]
-}

