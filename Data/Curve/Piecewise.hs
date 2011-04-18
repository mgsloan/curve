-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Piecewise 
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Piecewise function representation (of homogenous type).

{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-} 

module Data.Curve.Piecewise where

import Data.Curve.Classes
import Data.Curve.Interval ((...))
import qualified Data.Curve.Interval as I
import Data.Curve.Linear

import Algebra.Lattice
import Control.Arrow (first, second)
import Data.Maybe (fromJust)
import Data.VectorSpace
import Numeric.Rounding

-- (seg, time) means that the seg holds for prev_time <= t < time
data (Curve a) => Pw a = Pw { pwMin :: (Domain a)
                            , pwSegs :: [(a, Domain a)]
                            }

ivls :: (Curve a, Ord (Domain a)) => Pw a -> [(I.Interval (Domain a), a)]
ivls (Pw f xs) = helper f xs
  where helper f [] = []
        helper f ((x,t):xs) = (f ... t, x) : helper t xs

segs :: (Curve a) => Pw a -> [a]
segs = map fst . pwSegs

mapPw :: (Curve a, Curve b, Domain a ~ Domain b) => (a -> b) -> Pw a -> Pw b
mapPw f pw = Pw (pwMin pw) $ map (first f) (pwSegs pw)

tagged :: (Ord (Domain a), Curve a) => Pw a -> [(I.Interval (Domain a), (I.Interval (Domain a), a))]
tagged = map (\(x, y) -> (x, (x, y))) . ivls

{-
zipPw :: (Portionable a, Portionable b, Curve c, RealFloat (Domain a),
  Eq a, Eq b,
  Fractional (Domain c), Ord (Domain c),
  Domain a ~ Domain c, Domain b ~ Domain c) => 
  (a -> b -> c) -> Pw a -> Pw b -> Pw c
-}
zipPw :: (Portionable a, Precision (Domain a), Eq a, DomainBounds a ~ I.Interval (Domain a)) => 
  (a -> a -> a) -> Pw a -> Pw a -> Pw a
zipPw f a b = Pw fr . map process $ I.overlapsBetween (tagged a) (tagged b)
  where process (ivl, (i1, x1), (i2, x2)) = (f (por i1 x1) (por i2 x2), I.sup ivl)
          where por i = portion (I.localTo ivl i)
        fr = max (pwMin a) (pwMin b)

-- TODO: use better transformation representation
transDomain :: (Curve a, RealFloat (Domain a)) => Linear (Domain a) -> Pw a -> Pw a
transDomain l@(Linear f t) (Pw fr xs)
    | f == t = Pw 0 []
    | f  < t = Pw (l`at`fr) $ map (second (l`at`)) xs
    | f  > t = helper fr xs []
  where helper prev [] acc = Pw prev acc
        helper prev ((x, nxt):xs) acc = helper nxt xs ((x, l`at`prev):acc)

joinPw :: (Curve a, Precision (Domain a)) => Pw (Pw a) -> Pw a
joinPw (Pw f xs) = undefined {- Pw f (helper f xs)
  where helper f ((x, t):xs) = clipDomain unitivl x

clipDomain :: (Portionable a) => DomainBounds a -> Pw a -> Pw a
clipDomain ivl = undefined
-}

instance (Curve a, Precision (Domain a)) => Curve (Pw a) where
    type Domain (Pw a) = Domain a
    type Codomain (Pw a) = Maybe (Codomain a)

    at (Pw f xs) t | t < f = Nothing
                   | otherwise = helper f xs
      where helper _ [] = Nothing
            helper fr ((x, to):xs) | fr == t = Just $ x `at` fr
                | t < to = Just $ x `at` I.mapping (fr ... to) I.unit t
                | otherwise = helper to xs

instance (IsFinite a, Curve a) => IsFinite (Pw a) where
    isFinite   = all isFinite   . segs
instance (IsZero a, Curve a)   => IsZero   (Pw a) where
    isZero     = all isZero     . segs

constantPw = Pw ((-1) / 0) . maybe [] (\x -> [(Linear x x, 1 / 0)])

instance (FunctionBounds a, Portionable a, 
  Precision (Domain a), Precision (Codomain a),
  CodomainBounds a ~ I.Interval (Codomain a)) 
  => FunctionBounds (Pw a) where
    type DomainBounds (Pw a) = I.Interval (Domain a)
    type CodomainBounds (Pw a) = I.Interval (Codomain a)
    domain (Pw f xs) = f ... (snd $ last xs)
    --TODO: don't use portion.
    bounds           i = foldl1 union . map (\x -> bounds (domain x) x) . segs . portion i
    boundsApprox eps i = foldl1 union . map (\x -> boundsApprox eps (domain x) x) . segs . portion i

instance (FunctionBounds (Pw a)) => Portionable (Pw a) where
    portion = undefined

instance (AdditiveGroup a, Portionable a, Precision (Domain a), 
  DomainBounds a ~ I.Interval (Domain a), Eq a)
 => AdditiveGroup (Pw a) where
    zeroV = Pw ((-1) / 0) [(zeroV, 1 / 0)]
    (^+^) = zipPw (^+^)
    negateV = mapPw negateV

instance (VectorSpace a, Precision (Domain a), Portionable a,
  DomainBounds a ~ I.Interval (Domain a), Eq a)
 => VectorSpace (Pw a) where
    type Data.VectorSpace.Scalar (Pw a) = Scalar a
    (*^) s = mapPw (s*^)

instance (Multiplicable a, Precision (Domain a), Portionable a,
  DomainBounds a ~ I.Interval (Domain a), Eq a)
 => Multiplicable (Pw a) where
    (^*^) = zipPw (^*^)

instance (Dividable a, Precision (Domain a), Portionable a,
  DomainBounds a ~ I.Interval (Domain a), Eq a)
 => Dividable (Pw a) where
    (^/^) = zipPw (^/^)

instance (Offsetable a, Precision (Domain a))
 => Offsetable (Pw a) where
    offset (Just x) = mapPw (offset x)
    offset Nothing = id

{-
arcLength :: (InnerSpace (Codomain a),
  DomainBounds a ~ Interval (Domain a), Eq a)
 => a -> a
arcLength = 
-}

{-
instance (Composable a b, InverseBounds b) => Composable (Pw a) (Pw b) where
    type CompositionType (Pw a) (Pw b) = Pw (CompositionType a b)
    compose a = joinPw $ mapPw process
      where process :: a -> Pw (CompositionType a b)
            process x = domainBounds x

instance (Rootable a) => Rootable (Pw a) where
    rootsAt pw x = map (\i s -> ivlMapping unitivl i $ rootsAt s x) $ ivls pw
-}
