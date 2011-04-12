module Data.Curve.Periodic where

import Data.Curve.Classes
import Data.Curve.Interval
import Data.Curve.Linear
import Data.Curve.SBasis
import Data.Curve.Util

import Data.VectorSpace

data (FunctionBounds a) => Periodic a = Periodic a (DomainBounds a)

-- Make offset smallest possible positive.
normalize :: Periodic a -> Periodic a
normalize (Periodic x ivl) = Periodic x (ivl `offsetInterval` delta)
  where delta = negate . (*(extent ivl)) . floor $ from ivl / extent ivl

instance (FunctionBounds a, CodomainBounds ~ Interval b) => Curve (Periodic a) where
    at (Periodic x ivl) = (x`at`) . ivlWrap ivl
    isZero     (Periodic x _) = isZero x
    isConstant (Periodic x _) = isConstant x
    isFinite   (Periodic x _) = isFinite x
    reverseDomain (Periodic x ivl) = Periodic (reverseDomain x) (negate ivl)
    constCurve val = Periodic (constCurve val) unitivl

doBounds f (Periodic x rep) ivl@(Interval fr to)
      | extent ivl > extent rep = f x unitivl
      | otherwise = f x (Interval high 1) `union` f x (Interval 0 low)
  where wfr = ivlWrap rep fr
        wto = ivlWrap rep to 
        low = min wfr wto
        high = max wfr wto

instance (FunctionBounds a) => FunctionBounds (Periodic a) where
    domain = infivl
    bounds = doBounds bounds
    boundsApprox = doBounds boundsApprox

mapPeriodic :: (Curve a) => (a -> a) -> (Periodic a) -> (Periodic a)
mapPeriodic f (Periodic x ivl) = Periodic (f x) ivl

{-
zipBeats :: (Curve a) => (a -> a -> a) -> (Periodic (Pw a)) -> (Periodic (Pw a)) -> (Periodic (Pw a))
zipBeats f (Periodic x xivl) (Periodic y yivl) = Periodic (concatPw segs) ivl
  where ivl = offsetInterval (from xivl) (Interval 0 period)
        period = 1 / abs (1 / extent xivl - 1 / extent yivl)
        offsets = map (ivlWrap xivl) $ iterate (+(extent yivl)) 0
        segs = 
-}

instance (FunctionBounds a) => AdditiveGroup (Periodic (Pw a))
    zeroV = Periodic zeroV unitivl
    (^+^) = zipBeats (^+^) ()
    negateV = mapPeriodic negateV

instance (FunctionBounds a) => VectorSpace (Periodic a) where
    type Scalar (Pw a) = Scalar a
    (*^) s = mapPeriodic (s*^)

instance (FunctionBounds a, Multiplicable a) => Multiplicable (Periodic a) where
    (^*^) = zipBeats (^*^)

instance (Portionable a, DomainBounds b) => Composable (Periodic a) (Periodic b) where
    type CompositionType a b 
    compose a (Periodic b ivl) = Periodic (map (uncurry portion) ivls) ivl
      where ivls = -- TODO: determine how to split function up by evenly-spaced roots

instance (FunctionBounds a, Dividable a) => Dividable (Periodic a) where
    reciprocal = mapPeriodic reciprocal
    (^/^) = zipBeats (^/^)

tesselate :: (VectorSpace a, Offsetable a) => a

instance (Curve a) => Rootable a where
    rootsAt (Periodic x rep) y = tesselate rep (rootsAt x y)

instance (DomainBounds a) => DomainBounds (Periodic a)
    domainBounds (Periodic x rep) y = tesselate rep (domainBounds x y)

{- TODO need portion type
instance (Portionable a) => Portionable (Periodic a) where
-}

{-
instance (Rootable a) => Rootable (Periodic a) where
    rootsAt (Periodic x rep) y = rootsAt x y
-}
