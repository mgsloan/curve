{-# LANGUAGE NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Rect
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Functions to deal with the specific case of (Interval a, Interval a)

module Data.Curve.Rect where

import Data.Curve.Interval
import Data.Curve.Linear
import Data.Curve.Util
import Data.VectorSpace
import Numeric.Rounding

type DRect = (Interval Double, Interval Double)
type FRect = (Interval Float,  Interval Float)

emptyR :: (Precision a) => (Interval a, Interval a)
emptyR = (empty, empty)

middle :: (Precision a) => (Interval a, Interval a) -> (a, a)
middle = mapT midpoint

-- Clockwise around corners
corner :: (Integral a, Precision b) => a -> (Interval b, Interval b) -> (b, b)
corner 0 (I x1  _, I y1  _) = (runDown x1, runDown y1)
corner 1 (I  _ x2, I y1  _) = (runUp   x2, runDown y1)
corner 2 (I  _ x2, I  _ y2) = (runUp   x2, runUp   y2)
corner 3 (I x1 x2, I  _ y2) = (runDown x1, runUp   y2)
corner x r = corner (x `mod` 4) r

-- TODO: place this in a Line.hs module orso
lineBetween :: (a, a) -> (a, a) -> (Linear a, Linear a)
lineBetween (x1, y1) (x2, y2) = (Linear x1 x2, Linear y1 y2)

rside :: (Integral a, Precision b) => a -> (Interval b, Interval b) -> (Linear b, Linear b)
rside i r = lineBetween (corner i r) (corner (i + 1) r)

boundPoints :: (Precision a, Precision a1) => [(a1, a)] -> (Interval a1, Interval a)
boundPoints xs = (fromList $ map fst xs, fromList $ map snd xs)

fromCorners :: (Precision a1, Precision a) => (a1, a) -> (a1, a) -> (Interval a1, Interval a)
fromCorners c1 c2 = boundPoints [c1, c2]

rect :: (Precision a, Precision a1, AdditiveGroup a1, AdditiveGroup a) =>
        (a1, a) -> (a1, a) -> (Interval a1, Interval a)
rect tl sz = fromCorners tl $ tl ^+^ sz

clampR :: (Precision a) => (Interval a, Interval a) -> (a, a) -> (a, a)
clampR = zipT clamp

extendR :: (Precision a) => (Interval a, Interval a) -> (a, a) -> (Interval a, Interval a)
extendR = zipT extend

extentR :: (Precision a) => (Interval a, Interval a) -> (a, a)
extentR = mapT extent

expandR :: (Precision a) => (a, a) -> (Interval a, Interval a) -> (Interval a, Interval a)
expandR = zipT expand
