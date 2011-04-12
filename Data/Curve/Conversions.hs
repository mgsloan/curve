-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Conversions
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Conversions between curve types.

{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-} 

module Data.Curve.Conversions where

import Data.Curve.Bezier
import Data.Curve.Classes
import Data.Curve.Const
import Data.Curve.Linear
import Data.Curve.Poly
import Data.Curve.SBasis
import Data.Curve.Util

import Data.AdditiveGroup

-- Constant conversion instances

instance ToBezier (Const a) where
    type BezierType (Const a) = Bezier a
    toBezier (Const x) = Bezier [x]

instance (Num a) => ToSBasis (Const a) where
    type SBasisType (Const a) = SBasis a
    toSBasis x = SBasis [toLinear x]

instance ToPoly (Const a) where
    type PolyType (Const a) = Poly a
    toPoly (Const x) = Poly [x]

-- SBasis conversion instances

instance (Num a) => ToBezier (SBasis a) where
    type BezierType (SBasis a) = Bezier a
    toBezier = undefined

instance (Num a) => ToSBasis (SBasis a) where
    type SBasisType (SBasis a) = SBasis a
    toSBasis = id

instance (Num a) => ToPoly (SBasis a) where
    type PolyType (SBasis a) = Poly a
    toPoly = undefined

-- Bezier conversion instances 

instance (Num a) => ToBezier (Bezier a) where
    type BezierType (Bezier a) = Bezier a
    toBezier = id

instance (RealFloat a) => ToSBasis (Bezier a) where
    type SBasisType (Bezier a) = SBasis a
    toSBasis b = helper (order b) (bezierCoefs b)
      where helper 0 _ = SBasis []
            helper 1 (x:xs) = SBasis [Linear x x]
            helper 2 (x:y:xs) = SBasis [Linear x y]
            helper i xs = up ^*^ helper (i - 1) xs ^+^
                          dn ^*^ helper (i - 1) (tail xs)
            up = SBasis [Linear 0 1]
            dn = SBasis [Linear 1 0]

instance (Num a) => ToPoly (Bezier a) where
    type PolyType (Bezier a) = Poly a
    toPoly = undefined

-- Linear conversion instances

instance (Num a) => ToBezier (Linear a) where
    type BezierType (Linear a) = Bezier a
    toBezier (Linear fr to) = Bezier [fr, to]

instance (Num a) => ToSBasis (Linear a) where
    type SBasisType (Linear a) = SBasis a
    toSBasis l = SBasis [l]

instance (Num a) => ToPoly (Linear a) where
    type PolyType (Linear a) = Poly a
    toPoly (Linear fr to) = Poly [fr, to - fr]

-- D2 conversion instances

instance (ToBezier a) => ToBezier (a, a) where
    type BezierType (a, a) = (BezierType a, BezierType a)
    toBezier = mapT toBezier

instance (ToSBasis a) => ToSBasis (a, a) where
    type SBasisType (a, a) = (SBasisType a, SBasisType a)
    toSBasis = mapT toSBasis

instance (ToPoly a) => ToPoly (a, a) where
    type PolyType   (a, a) = (PolyType a,   PolyType a)
    toPoly = mapT toPoly
