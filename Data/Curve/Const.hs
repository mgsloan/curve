
{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances,
MultiParamTypeClasses #-} 

module Data.Curve.Const where

import Data.Curve.Classes
import qualified Data.Curve.Interval as I
import Data.Curve.Util

import Numeric.Rounding
import Data.VectorSpace

data Const a = Const {constValue :: a} deriving (Show)

-- lift1
cmap :: (a -> a) -> Const a -> Const a
cmap f (Const x) = Const (f x) 

-- lift2
czip :: (a -> a -> a) -> Const a -> Const a -> Const a
czip f (Const a) (Const b) = Const $ f a b

const2 f = flip $ const $ f

instance (Num a) => Curve (Const a) where
    type Domain (Const a) = a
    type Codomain (Const a) = a

    at = const2 constValue

instance (IsFinite a) => IsFinite (Const a) where
    isFinite = isFinite . constValue

instance (IsZero a) => IsZero (Const a) where
    isZero = isZero . constValue

instance (Num a, Precision a) => 
  FunctionBounds (Const a) where
    type DomainBounds (Const a) = I.Interval a
    type CodomainBounds (Const a) = I.Interval a

    domain _ = I.unit
    bounds _ = I.singleton . constValue

instance (Precision a) => Portionable (Const a) where
    portion = const id

instance (Num a) => Offsetable (Const a) where
    offset x = cmap (+x)

instance (Num a)  => AdditiveGroup (Const a) where
    zeroV = Const 0 
    (^+^) = czip (+)
    negateV = cmap (negate)

instance (Num a) => VectorSpace (Const a) where
   type Scalar (Const a) = a
   (*^) s = cmap (s*)

instance (Num a) => Composable (Const a) (Const a) where
    type CompositionType (Const a) (Const a) = Const a
    compose = const2 $ id
