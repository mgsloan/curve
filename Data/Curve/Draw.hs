-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Drawing
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Implementation of cairo Curve drawing functions

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances,
FlexibleContexts, UndecidableInstances, ExistentialQuantification #-} 

module Data.Curve.Draw
  ( Draw(..)
  , Color(..)
  , move, relMove
  ) where

import Data.Curve.Bezier
import Data.Curve.Classes
import Data.Curve.Conversions
import qualified Data.Curve.Interval as I
import Data.Curve.Linear
import Data.Curve.Piecewise
import Data.Curve.SBasis
import Data.Curve.Util

import qualified Graphics.Rendering.Cairo as C

move = uncurry C.moveTo
relMove = uncurry C.relMoveTo

--TODO: Arc?

data Drawable = forall a. Draw a => Drawable a

class Draw a where
    draw :: a -> C.Render ()

instance Draw Drawable where
    draw (Drawable x) = draw x

instance Draw (Linear Double, Linear Double) where
    draw (Linear x1 x2, Linear y1 y2) = C.moveTo x1 y1 >> C.lineTo x2 y2

instance Draw (Bezier Double, Bezier Double) where
    draw (Bezier [x1, x2], Bezier [y1, y2])         = C.moveTo x1 y1 >> C.lineTo x2 y2
    draw (Bezier [x1, x2, x3], Bezier [y1, y2, y3]) = C.moveTo x1 y1 >>
        C.curveTo ((x22 + x1) / 3) ((y22 + y1) / 3) ((x22 + x3) / 3) ((y22 + y3) / 3) x3 y3
      where x22 = 2 * x2
            y22 = 2 * y2
    draw (Bezier [x1, x2, x3, x4], Bezier [y1, y2, y3, y4]) =
        C.moveTo x1 y1 >> C.curveTo x2 y2 x3 y3 x4 y4
    draw _ = undefined

-- instance (ToBezier a, Drawable (BezierType a, BezierType a)) => Drawable (a, a) where
--    draw (x, y) = draw (toBezier x, toBezier y)

instance Draw (I.Interval Double, I.Interval Double) where
    draw (x, y) = C.rectangle (I.inf x) (I.inf y) (I.extent x) (I.extent y)

class Color a where
    setColor :: a -> C.Render ()

instance Color (Double, Double, Double) where
    setColor = uncurry3 C.setSourceRGB

instance Color (Double, Double, Double, Double) where
    setColor = uncurry4 C.setSourceRGBA

instance Color (Int, Int, Int) where
    setColor = uncurry3 C.setSourceRGB . mapT ((/255.0) . fromIntegral)

instance Color (Int, Int, Int, Int) where
    setColor = uncurry4 C.setSourceRGBA . mapT ((/255.0) . fromIntegral)

{-
instance (Drawable (a, a), Portionable a) => Drawable (Pw a, Pw a) where
    draw (x, y) = map draw $ pwSegs $ zipPw (,) x y

instance (Drawable (a, a)) => Drawable (Pw (a, a)) where
    draw = mapM_ draw . pwSegs
-}
