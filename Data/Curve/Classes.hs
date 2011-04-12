-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Classes
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- 

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts,
UndecidableInstances, FlexibleInstances #-} 
module Data.Curve.Classes
  ( Predicate(..)
  , union, intersect
  , Curve(..) 
  , IsZero(..)
  , IsFinite(..)
  , FunctionBounds(..)
  , Portionable(..)
  , InverseBounds(..)
  , Rootable(..)
  , Invertible(..)
  , Integrable(..)
  , Composable(..)
  , Multiplicable(..)
  , Dividable(..)
  , Offsetable(..)
  , ToSBasis(..)
  , ToBezier(..)
  , ToPoly(..)
  ) where

import Algebra.Lattice

-- | General class for objects which have predicate-like properties, i.e.
-- implement a set of "Element a".
class Predicate a where
    type Element a
    element :: Element a -> a -> Bool

-- | Union on predicate-set, defined when JoinSemiLattice is implemented.
union :: (JoinSemiLattice a, Predicate a) => a -> a -> a
union = join

-- | Intersect on predicate-set, defined when MeetSemiLattice is implemented.
intersect :: (MeetSemiLattice a, Predicate a) => a -> a -> a
intersect = meet

-- | Curves are representations of (usually / mostly continuous) functions
-- from a domain to a codomain.
class Curve a where
    type Domain a
    type Codomain a
    at :: a -> (Domain a) -> (Codomain a)

-- | Predicate for determining whether an object (scalar, curve, etc) is
-- restricted to a finite codomain.
class IsFinite a where
    isFinite :: a -> Bool

instance IsFinite Float where
  isFinite x | isInfinite x || isNaN x = False 
  isFinite _ = True
instance IsFinite Double where
  isFinite x | isInfinite x || isNaN x = False 
  isFinite _ = True

-- | Represents the ability to query whether an object (scalar, curve, etc)
-- is zero for the entire domain.  If an instance of Constant also exists,
-- then (isZero x) implies (isConstant x)
class IsZero a where
  isZero :: a -> Bool

instance IsZero Float  where isZero = (0==)
instance IsZero Double where isZero = (0==)

-- Class representing the ability to query bounds of the codomain, given
-- bounds of the domain.  In other words, this allows you to answer the
-- question "what are the range of values given this range of inputs?".
class (Curve a,  Predicate (CodomainBounds a)) => FunctionBounds a where
    type DomainBounds a
    type CodomainBounds a
    domain :: a -> DomainBounds a
    bounds       :: DomainBounds a -> a -> CodomainBounds a
    bounds = boundsApprox 0
    boundsApprox :: Double -> DomainBounds a -> a -> CodomainBounds a
    boundsApprox = const bounds

-- | Represents the ability to extract a portion of the curve, with the
-- same type as a result.  This can be conceptualized as remapping &
-- cropping the  passed range to the full domain of the result
class (Curve a) => Portionable a where
    portion :: DomainBounds a -> a -> a

-- | Inverse of FunctionBounds.  This allows querying for the set of
-- ranges on the domain which yield values of a particular codomain range.
class (FunctionBounds a) => InverseBounds a where
    domainBounds :: a -> CodomainBounds a -> [DomainBounds a]

-- | This is essentially the inverse of `at`.  Given values of the
-- codomain, yields all values in the domain which could result in it.
-- For contiguous sections of the queried value, nothing is returned.  This
-- should probably be fixed by having a similar function -> [DomainBounds a].
-- In order to work around this, InverseBounds with a narrow range can be
-- used.
class (Curve a) => Rootable a where
    rootsAt :: a -> Codomain a -> [Domain a]

-- TODO: The type to represent simultaneous functions would be
-- ([(Interval, a)], [Domain a, Interval (Codomain a)])

-- Represents the capability to retrieve a representation for the inverse
-- of the curve. TODO: constraints on InverseType?
class (Curve a) => Invertible a where
    type InverseType a
    inverse :: a -> InverseType a

-- | Represents the ability to perform closed integral / derivative.
class (Curve a) => Integrable a where
    integralOrder, derivativeOrder :: Int -> a -> a
    integral, derivative :: a -> a
    integralOrder n   = derivativeOrder (negate n)
    derivativeOrder n = integralOrder (negate n)
    integral   = integralOrder 1
    derivative = derivativeOrder 1

    --integralOrder n = nest n integral
    --derivativeOrder n = nest n derivative

-- | Represents the capability to compose two curve-types, e.g. if f and g
-- are curves, then (at (compose f g)) == (at f) . (at g)
class (Curve a, Curve b {- Domain a ~ Codomain b -} ) => Composable a b where
    type CompositionType a b
    compose :: a -> b -> CompositionType a b

-- | Closed multiplication.
class Multiplicable a where
    (^*^) :: a -> a -> a

-- | Closed division / reciprocal.
class (Multiplicable a) => Dividable a where
    reciprocal :: a -> a
    (^/^) :: a -> a -> a
    a ^/^ b = a ^*^ (reciprocal b)

-- | Yields a representation of the output-offset of a curve.
class (Curve a) => Offsetable a where
    offset :: Codomain a -> a -> a

class ToSBasis a where
    type SBasisType a
    toSBasis :: a -> SBasisType a

class ToBezier a where
    type BezierType a
    toBezier :: a -> BezierType a

class ToPoly a where
    type PolyType a
    toPoly :: a -> PolyType a
