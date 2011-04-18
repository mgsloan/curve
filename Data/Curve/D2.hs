-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.D2
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Instances which implement lifting 'x' to two dimensional variant (x, x)

{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses,
FlexibleContexts #-}

module Data.Curve.D2 where

import Data.Curve.Classes
import Data.Curve.Util

import Algebra.Lattice
import Data.Maybe (fromJust)
import Data.VectorSpace
import Control.Arrow ((***), (&&&))


--instance (Set a, Set b) => Set (a, b) where
--  type Element (a, b) = (Element a, Element b)
--  union (x, y) = (union x) *** (union y)
instance (Predicate a, Predicate b) => Predicate (a, b) where
  type Element (a, b) = (Element a, Element b)
  element (x, y) (a, b)= x `element` a && y `element` b

instance (Curve a) => Curve (a, a) where
  type Domain (a, a) = Domain a
  type Codomain (a, a) = (Codomain a, Codomain a)
  at (f, g) = (at f) &&& (at g)

instance (IsZero a) => IsZero (a, a) where
  isZero     = foldT (&&) . mapT isZero

instance (IsFinite a) => IsFinite (a, a) where
  isFinite   = foldT (&&) . mapT isFinite

instance (FunctionBounds a, 
    Predicate (DomainBounds a), MeetSemiLattice (DomainBounds a)) => 
    FunctionBounds (a, a) where
  type DomainBounds (a, a) = DomainBounds a
  type CodomainBounds (a, a) = (CodomainBounds a, CodomainBounds a)
  domain = foldT intersect . mapT domain
  bounds           d = mapT $ bounds           d
  boundsApprox eps d = mapT $ boundsApprox eps d

{- TODO it isn't quite this simple.
instance (InverseBounds a) => InverseBounds (a, a) where
  domainBounds (f, g) = (domainBounds f) &&& (domainBounds g)
instance (Rootable a) => Rootable (a, a) where
  rootsAt (f, g) = foldT (++) . (rootsAt f &&& rootsAt g)

instance (Invertible a) => Invertible (a, a) where
 -}

instance (Differentiable a) => Differentiable (a, a) where
  derivative        = mapT derivative
  derivativeOrder n = mapT (derivativeOrder n)

instance (Integrable a) => Integrable (a, a) where
  integral          = mapT integral
  integralOrder n   = mapT (integralOrder n)

instance Composable a b => Composable (a, a) b where
  type CompositionType (a, a) b = (CompositionType a b, CompositionType a b)
  compose t x = mapT (`compose` x) t

instance Multiplicable a => Multiplicable (a, a) where
  (^*^) = zipT (^*^)

instance Dividable a => Dividable (a, a) where
  reciprocal = mapT reciprocal
  (^/^) = zipT (^/^)

instance Offsetable a => Offsetable (a, a) where
  offset (x, y) (a, b) = (offset x a, offset y b)

dot :: (Multiplicable a, AdditiveGroup a) => 
  (a, a) -> (a, a) -> a
dot a b = foldT (^+^) (a ^*^ b)

rotCW, rotCCW :: (AdditiveGroup a) => (a, a) -> (a, a)
rotCW (x, y) = (negateV y, x)
rotCCW (x, y) = (y, negateV x)


--rot :: (VectorSpace a) => Scalar a -> (a, a)
