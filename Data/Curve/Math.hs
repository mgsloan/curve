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
 ( isConstant
 , extrema, extremaBounds, criticalPoints
 , nearest, nearestPw
 , dist, normalize, arcLength, unitSpeed, curveOffset, curveAlong
 , toPolar, fromPolar, toPolarPnt, fromPolarPnt
 , lineInterTime, linInterTime, segInterTime, raySegInterTime
 ) where

import Control.Arrow (first, second, (&&&))
import Control.Monad (mfilter)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (minimumBy, sortBy)
import Data.VectorSpace
import Data.NumInstances

import Data.Curve.Classes
import Data.Curve.D2 (dot, rotCW)
import Data.Curve.Linear
import Data.Curve.Piecewise
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
criticalPoints :: (Curve a, Rootable a, Differentiable a, Num (Codomain a), Show (Domain a)) =>
  a -> [Domain a]
criticalPoints = (`rootsAt` 0) . derivative

nearestPw c p = mapdom . minimumBy (comparing (distanceSq p.snd.snd))
              . map (second (`nearest` p)) $ ivls c
  where mapdom (dom, (t, p)) = (I.mapping I.unit dom t, p)

-- | The nearest point of a 2D function with unit domain.
nearest c p = fst $ nearestFurthest I.unit c p

-- | Finds the nearest and furthest locations on a curve, from a given 2D
-- point.  The results are represented as two pairs of (Domain a, Codomain (a, a))
nearestFurthest :: (Curve a, Rootable a, Differentiable a, Multiplicable a,
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

dist (x, y) = pow 0.5 $ (pow 2 x) ^+^ (pow 2 x)
--dist (x, y) = pow 0.5 $ x ^*^ x ^+^ y ^*^ y

normalize p = p ^/^ (d, d)
  where d = dist p

arcLength :: (Integrable a, AdditiveGroup a, Pow a, Fractional (Codomain a))
  => (a, a) -> a
arcLength = integral . dist . derivative

unitSpeed :: (Integrable a, AdditiveGroup a, Pow a,
  Invertible a, Fractional (Codomain a), Composable (a, a) (InverseType a))
  => (a, a) -> CompositionType (a, a) (InverseType a)
unitSpeed p = compose p . inverse $ arcLength p

curveOffset x p = x  *^ (normalize . rotCW $ derivative p) ^+^ p
curveAlong  c p = c ^*^ (normalize . rotCW $ derivative p) ^+^ p


toPolar :: (Trig a, Pow a, AdditiveGroup a, Fractional (Codomain a), Dividable a) =>
  (a, a) -> (a, Angle a)
toPolar = dist &&& (arctangent . uncurry (^/^))
fromPolar :: (Trig a, Multiplicable a) =>
  (a, Angle a) -> (a, a)
fromPolar (d, t) = (cosine t ^*^ d, sine t ^*^ d)

-- Temporary. TODO: remove
toPolarPnt :: (Double, Double) -> (Double, Double)
toPolarPnt = (\(x, y) -> sqrt $ x * x + y * y) &&& uncurry atan2
fromPolarPnt (d, t) = (cos t * d, sin t * d)

-- | Determinant of the matrix
-- [ a b ]
-- [ c d ]
det a b c d = a * d - b * c
detpnt (a, b) (c, d) = det a b c d

lineInterTime o1 v1 o2 v2
  --TODO: maybe check if detpnt v1 v2 `isNear` 0 ?
    | isNaN sc  = Nothing
    | otherwise = Just $ sc *^ (detpnt b v2, detpnt b v1)
  where sc = recip $ detpnt v1 v2
        b  = o2 ^-^ o1

linInterTime l1 l2 = lineInterTime o1 v1 o2 v2
  where v1 = mapT lindelt l1; v2 = mapT lindelt l2
        o1 = l1 `at` 1; o2 = l2 `at` 1

--TODO: special cases for initial / final points.  Detect infinite soln.
segInterTime l1 l2 =
  mfilter (\(t1, t2) -> t1 < 0 || t1 > 1 || t2 < 0 || t2 > 1)
          (linInterTime l1 l2)

raySegInterTime l1 l2 =
  mfilter (\(t1, t2) -> t1 < 0 || t2 < 0 || t2 > 1)
          (linInterTime l1 l2)
