
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


extrema dom a = extremaBy id dom a $ criticalPoints a

extremaBounds dom = foldT (I....) . mapT snd . extrema dom

extremaBy f dom a = (head &&& last) .
  sortBy (comparing (f.snd)) . map (evalTag a) . 
  ((I.inf dom:).(I.sup dom:)) . filter (`element` dom)

evalTag c x = (x, c `at` x)

criticalPoints :: (Curve a, Rootable a, Integrable a, Num (Codomain a), Show (Domain a)) =>
  a -> [Domain a]
criticalPoints = (`rootsAt` 0) . derivative

nearest c p = fst $ nearestFurthest I.unit c p

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

--nearest_seg :: (Linear a, Linear a) -> a -> Double
--nearest_seg line = 

--TODO: assumption of sampling at 0 / 1 valid?

{-
class (Curve a, Rootable a,  Integrable a, 
  Multiplicable a, AffineSpace a, AdditiveGroup a, 
  Num (Domain a), Num (Codomain a), Ord (Scalar a),

  (Ord (Scalar (Diff (Codomain a)))),
  InnerSpace (Diff (Codomain a)),
  AdditiveGroup (Scalar (Diff (Codomain a))),
  AffineSpace (Codomain a)) =>
    Nearable a where
  nearest2 :: (Diff a ~ Codomain a) => (a, a) -> Diff (a, a) -> (Domain a, Codomain (a, a))
  nearest2 c p
    | not (isConstant c) = minimumBy (comparing (distanceSq p . snd)) conv
    | otherwise = c `at` 0
    where deriv = derivative c
          pnts = (`rootsAt` 0) $ (c .-^ p) `dot` deriv
          conv =  map (\x -> (x, c `at` x)) $ [0, 1] ++ pnts

-}



{-
nearest c p | not (isConstant c) =
   minimumBy (comparing $ distanceSq p) . map (c `at`) .
   (0:) . (1:) . (`rootsAt` 0) $ (c ^-^ p) <.> deriv
  | otherwise = c `at` 0
  where deriv = derivative c
-}
