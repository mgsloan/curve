-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.SBasis
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- 1D s-power basis curve representation.

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-} 

module Data.Curve.SBasis where

import Data.Curve.Classes
import Data.Curve.Const
import Data.Curve.Function
import qualified Data.Curve.Interval as I
import Data.Curve.Linear
import Data.Curve.Math
import Data.Curve.Util

import Numeric.Rounding
import Data.AdditiveGroup
import Data.VectorSpace
import Data.List

data SBasis a = SBasis { sbasisCoefs :: [Linear a] } deriving (Eq, Show)

modCoefs f (SBasis xs) = SBasis (f xs)

-- Not really that useful for sbasis.
-- instance Functor SBasis where fmap f = modCoefs (map (fmap f))

lift1 f = modCoefs (map f)

lift2 f d (SBasis xs) (SBasis ys) = SBasis $ zipWithDefault f d xs ys

normalizeSB :: (IsZero a) => SBasis a -> SBasis a
normalizeSB = modCoefs (reverse . dropWhile isZero . reverse)

instance (Num a) => Curve (SBasis a) where
    type Domain (SBasis a) = a
    type Codomain (SBasis a) = a

    at (SBasis []) _ = 0
    at (SBasis (x:_)) 0 = at x 0
    at (SBasis (x:_)) 1 = at x 1
    at (SBasis xs) t = foldl1 (+) . map (`at`t) $ zipWith (*^) (sunfoldr (*s) 1) xs
      where s = (1 - t) * t

instance (IsFinite a) => IsFinite (SBasis a) where
    isFinite = all isFinite . sbasisCoefs

instance (IsZero a) => IsZero (SBasis a) where
    isZero = all isZero . sbasisCoefs

instance (Precision a, IsZero a) => FunctionBounds (SBasis a) where
    type DomainBounds (SBasis a)   = I.Interval a
    type CodomainBounds (SBasis a) = I.Interval a
    domain _ = I.unit
    bounds = extremaBounds
    --TODO: use eps?
    boundsApprox _ (I.I (Round t0) (Round t1)) = foldT (I....) . foldr step (0, 0) . sbasisCoefs
      where step l@(Linear a b) (x, y) = (app (x<0) min x, app (y>0) max y)
              where app c f v | not c || t < t0 || t > t1 = 
                        f (l `at` t0 + v * t0 * (1 - t0))
                          (l `at` t1 + v * t1 * (1 - t1))
                              | otherwise = Linear (a+v*t) b `at` t
                      where t | c = ((b - a) / v + 1) * 0.5
                              | otherwise = 0

instance (Precision a) => Portionable (SBasis a) where
    portion ivl = undefined

instance (Num a) => AdditiveGroup (SBasis a) where
    zeroV = SBasis []
    (^+^) = lift2 (^+^) (Linear 0 0)
    negateV = lift1 negateV

instance (Num a) => VectorSpace (SBasis a) where
    type Scalar (SBasis a) = a
    (*^) s = lift1 (s*^)

instance (Num a) => Offsetable (SBasis a) where
    offset o (SBasis []) = SBasis [Linear o o]
    offset o (SBasis (x:xs)) = SBasis ((offset o x):xs)

eachShift f xs ys = map (zipWith f xs) (inits ys)
linconst :: (RealFloat a) => a -> Linear a
linconst = toLinear . Const
sbconst :: (RealFloat a) => a -> SBasis a
sbconst x = SBasis [linconst x] 

instance (RealFloat a) => Multiplicable (SBasis a) where
    (SBasis xs) ^*^ (SBasis ys) = SBasis $ foldl1 (zipWith (^+^)) $ tris ++ muls
      where tris = map (map (linconst . negate)) $
                   eachShift (*) (map lindelt xs) (map lindelt ys)
            muls = eachShift (linzip (*)) xs ys
    mult1 = SBasis [Linear 1 1]

instance (RealFloat a) => Composable (SBasis a) (SBasis a) where
    type CompositionType (SBasis a) (SBasis a) = SBasis a
    compose (SBasis xs) b = foldr iter (sbconst 0) xs
      where s = sbconst 1 ^-^ b
            iter (Linear f t) r = r ^*^ s ^+^ sbconst f ^-^ (f *^ b) ^+^ (t *^ b)

indices0, indices1 :: RealFloat a => [a]
indices0 = map fromIntegral [0::Int ..]
indices1 = map fromIntegral [1::Int ..]

instance (RealFloat a, IsZero a) => Differentiable (SBasis a) where
    derivative (SBasis xs) = SBasis $ zipWith (\d -> lin2map (d+) (d-)) ds delts
      where ds = zipWith (\k -> ((2*k+1)*) . lindelt) indices0 xs
            delts = zipWith (*^) indices1 $ (++[toLinear$Const 0]) $ tail xs

instance (RealFloat a, IsZero a) => Integrable (SBasis a) where
    integral (SBasis xs) = normalizeSB . SBasis $ zipWith linmw mids delts
      where mids = (0:) . zipWith (\k -> (/(2 * k))) indices1 $ map lindelt xs
            delts = scanr (\(ix, l) c -> (linmean l + (ix+1) * c / 2) / (2 * ix + 1)) 0 $ zip indices0 xs
            linmw mid delt = Linear (mid - delt / 2) (mid + delt / 2)


-- TODO: better implementation
instance (Precision a) => Rootable (SBasis a) where
    rootsAt f = rootsAt $ at f
