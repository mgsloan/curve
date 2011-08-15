-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Interval
-- Copyright   :  (c) 2011 Michael Sloan, 2010 Edward Kmett
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Module to import to bring in most of the Data.Curve library.  Omits
-- Data.Curve.Interval (as it should usually be imported qualified), and 
-- Data.Curve.Util (which contains internal helpers which may as well be
-- exported).
--
-- Rather than having each curve type store 2D data, dimensional lifting is
-- provided (by Data.Curve.D2).  This means that all that are implemented
-- are 1D functions.

module Data.Curve
  ( module Data.Curve.Bezier
  , module Data.Curve.Classes
  , module Data.Curve.Const
  , module Data.Curve.Conversions
  , module Data.Curve.D2
  , module Data.Curve.Function
  , module Data.Curve.Linear
  , module Data.Curve.Rect
  , module Data.Curve.Math
  , module Data.Curve.Piecewise
  , module Data.Curve.Poly
  , module Data.Curve.SBasis
  -- module Data.Curve.Periodic
  -- module Data.Curve.Concat
  , module Data.VectorSpace
  ) where

import Data.Curve.Bezier
import Data.Curve.Classes
import Data.Curve.Const
import Data.Curve.Conversions()
import Data.Curve.D2
import Data.Curve.Function
import Data.Curve.Linear
import Data.Curve.Rect
import Data.Curve.Math
import Data.Curve.Piecewise
import Data.Curve.Poly
import Data.Curve.SBasis

import Data.VectorSpace
