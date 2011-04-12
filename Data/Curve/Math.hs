-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Math
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Generic functions / geometric operations on curves


{-# LANGUAGE FlexibleContexts, TypeFamilies #-} 

module Data.Curve.Math 
 ( isConstant, extrema, extremaBounds, criticalPoints, nearest
 ) where

import Control.Arrow((&&&))
import Data.Ord (comparing)
import Data.List (minimumBy, sortBy)
import Data.VectorSpace
import Data.NumInstances

import Data.Curve.Classes
import Data.Curve.D2 (dot)
import qualified Data.Curve.Interval as I
import Data.Curve.Util
import Numeric.Rounding

distanceSq a b = foldT (+) $ mapT (\x -> x * x) $ zipT (-) a b

isConstant :: (IsZero a, Offsetable a, Num (Codomain a), Num (Domain a)) => a -> Bool
isConstant c = isZero . (flip offset) c . negate $ c `at` 0

-- | The input and respective value of the extrema (minima and maxima of
-- the curve.  Assumes a continuous function, where the extrema necessarily
-- correspond to roots in the first derivative (critical points) 
extrema dom a = extremaBy id dom a $ criticalPoints a

-- | The interval bounds of a subset of a function, by using the extrema
-- function as described above.
extremaBounds dom = foldT (I....) . mapT snd . extrema dom

extremaBy f dom a = (head &&& last) .
  sortBy (comparing (f.snd)) . map (evalTag a) . 
  ((I.inf dom:).(I.sup dom:)) . filter (`element` dom)

evalTag c x = (x, c `at` x)

-- | The critical points of a differentiable function.
criticalPoints :: (Curve a, Rootable a, Integrable a, Num (Codomain a), Show (Domain a)) =>
  a -> [Domain a]
criticalPoints = (`rootsAt` 0) . derivative

-- | The nearest point of a 2D function with unit domain.
nearest c p = fst $ nearestFurthest I.unit c p

-- | Finds the nearest and furthest locations on a curve, from a given 2D
-- point.  The results are represented as two pairs of (Domain a, Codomain (a, a))
nearestFurthest :: (Curve a, Rootable a, Integrable a, Multiplicable a,
  AdditiveGroup a, Offsetable a, IsZero a,
  Precision (Domain a), Num (Domain a), Num (Codomain a), Ord (Codomain a),
  DomainBounds a ~ I.Interval (Domain a)) =>

  DomainBounds a -> (a, a) -> Codomain (a, a) -> 
  ((Domain a, Codomain (a, a)), (Domain a, Codomain (a, a)))

nearestFurthest ivl c p 
  | isZero c' = (c0, c0)
  | otherwise = extremaBy (distanceSq p) ivl c $
                (offset (negate p) c `dot` c') `rootsAt` 0
  where c' = derivative c
        c0 = (0, c `at` 0)
