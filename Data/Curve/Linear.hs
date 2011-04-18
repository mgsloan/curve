-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Linear
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Linear function representation.

{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances,
MultiParamTypeClasses #-} 

module Data.Curve.Linear where

import Data.Curve.Classes
import qualified Data.Curve.Interval as I
import Data.Curve.Util

import Numeric.Rounding
import Data.VectorSpace

data Linear a = Linear a a deriving (Show, Eq)

instance Functor Linear where
  fmap f (Linear fr to) = Linear (f fr) (f to)

lin2map :: (a -> a) -> (a -> a) -> Linear a -> Linear a
lin2map f1 f2 (Linear f t) = Linear (f1 f) (f2 t)

-- lift2
linzip :: (a -> a -> a) -> Linear a -> Linear a -> Linear a
linzip func (Linear f1 t1) (Linear f2 t2) = Linear (func f1 f2) (func t1 t2)

toLinear x = Linear (x `at` 0) (x `at` 1)

instance (Num a) => Curve (Linear a) where
    type Domain (Linear a) = a
    type Codomain (Linear a) = a

--    at :: Linear a -> Double -> a
    at (Linear f t) 0 = f
    at (Linear f t) 1 = t
    at (Linear f t) u = (1 - u) * f + u * t

instance (IsFinite a) => IsFinite (Linear a) where
    isFinite (Linear f t) = isFinite f && isFinite t

instance (IsZero a) => IsZero (Linear a) where
    isZero (Linear f t) = isZero f && isZero t

instance (Num a, Precision a) => 
  FunctionBounds (Linear a) where
    type DomainBounds (Linear a) = I.Interval a
    type CodomainBounds (Linear a) = I.Interval a

    domain _ = I.unit
    bounds dom l = I.fromList [l `at` I.inf dom, l `at` I.sup dom]

instance (Precision a) => Portionable (Linear a) where
    portion i l = Linear (l `at` I.inf i) (l `at` I.sup i)

instance (Num a) => Offsetable (Linear a) where
    offset x = fmap (+x)

instance (Num a)  => AdditiveGroup (Linear a) where
    zeroV = Linear 0 0 
    (^+^) = linzip (+)
    negateV = fmap negate

instance (Num a) => VectorSpace (Linear a) where
    type Scalar (Linear a) = a
    (*^) s = fmap (s*)

instance (Num a) => Composable (Linear a) (Linear a) where
    type CompositionType (Linear a) (Linear a) = Linear a
    compose f (Linear fr to) = Linear (f `at` fr) (f `at` to)

instance (Num a) => Differentiable (Linear a) where
    derivative (Linear f t) = Linear val val
      where val = t - f

lindelt (Linear f t) = t - f
linmean (Linear f t) = (t + f) / 2

{-
instance (RealFloat a) => InverseBounds (Linear a) where
    domainBounds l x = bounds x inv
      where inv = inverse l

instance (RealFloat a) => Rootable (Linear a) where
    rootsAt l x = rootsAt inv x
-}

