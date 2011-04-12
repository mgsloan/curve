-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Function
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Curve instances for the standard function type (a -> b)


{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances,
FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-} 

module Data.Curve.Function where

import Data.Curve.Classes
import qualified Data.Curve.Interval as I
import Data.Curve.Util

import Numeric.Rounding
import Data.VectorSpace
import Math.Root.Finder
import Math.Root.Finder.Brent

instance Curve (a -> b) where
    type Domain (a -> b) = a
    type Codomain (a -> b) = b

    at = ($) 

instance IsFinite (a -> b) where isFinite = const False
instance IsZero   (a -> b) where isZero   = const False

instance (Num a, Precision a, Precision b) => FunctionBounds (a -> b) where
    type DomainBounds (a -> b)   = I.Interval a
    type CodomainBounds (a -> b) = I.Interval b

    domain _ = I.unit
    bounds _ = undefined

instance (Precision a) => Portionable (a -> b) where
    portion i = (.(I.mapping i I.unit))

instance (Num b) => Offsetable (a -> b) where
    offset x = ((+x).)

instance Composable (b -> c) (a -> b) where
    type CompositionType (b -> c) (a -> b) = a -> c
    compose = (.)

findRoots :: (RealFloat a, Real a, Fractional a, Ord a, Precision a) => a -> (a -> a) -> a -> a -> [a]
findRoots eps f a b | abs (a - b) > eps = []
                    | otherwise = case brent f a b eps of
  Left x -> let est = estimateRoot x in [est] ++ 
                  findRoots eps f a (runDown $ down est - down eps) ++
                  findRoots eps f   (runUp $ up est + up eps) b
  Right x -> []

functionRoots f y = findRoots 0.001 (offset (-y) f) 0 1

instance (RealFloat a, Real a, Fractional a, Ord a, Precision a) => Rootable (a -> a) where
    rootsAt = functionRoots
