-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Curve.Util
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Utility functions for curve implementation.

{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses,
FlexibleContexts #-} 

module Data.Curve.Util where
import Data.List (unfoldr)
import Control.Arrow ((&&&), (***))
import Data.AffineSpace
import Data.VectorSpace
import Debug.Trace

near :: (AffineSpace a, InnerSpace (Diff a), Floating (Scalar (Diff a)), Ord (Scalar (Diff a)))
  => Scalar (Diff a) -> a -> a -> Bool
near eps x y = (x `distance` y) < eps

-- | Simple infinite unfold.
sunfoldr :: (a -> a) -> a -> [a]
sunfoldr f i = i : (unfoldr (Just . (f &&& f)) i)

-- | Similar to zipWith, except that a default value is provided, which
-- is used when one list or the other has run out.  Since only one default
-- value is provided, the two inputs must be of the same type.
zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault _ _ [] [] = []
zipWithDefault f a (x:xs) [] = f x a : zipWithDefault f a xs []
zipWithDefault f a [] (y:ys) = f a y : zipWithDefault f a [] ys
zipWithDefault f a (x:xs) (y:ys) = f x y : zipWithDefault f a xs ys

-- Currently unused
--scanM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
--scanM f x = foldM (\acc@(prev:_) val -> (f prev val) : acc) [x]

-- | This is a scan which seperates the concept of yielded value from the
-- state which is threaded through.
sweep :: (a -> b -> (b, c)) -> b -> [a] -> [c]
sweep mut x = map snd . scanl (\(b, c) a -> mut a b) (x, undefined)

projectT False (x, _) = x
projectT True  (_, x) = x

mutateT f False (x, y) = (f x, y)
mutateT f True (x, y) = (x, f y)

zipT :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipT f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

foldT :: (a -> b -> c) -> (a, b) -> c
foldT f (x, y) = f x y

--mapT f (a, b) = (f a, f b)

class MapT a b where
  type In  a :: *
  type Out b :: *
  mapT :: (In a -> Out b) -> a -> b

instance MapT (a, a) (b, b) where
  type In  (a, a) = a
  type Out (b, b) = b
  mapT f (a, b) = (f a, f b)

instance MapT (a, a, a) (b, b, b) where
  type In  (a, a, a) = a
  type Out (b, b, b) = b
  mapT f (a, b, c) = (f a, f b, f c)

instance MapT (a, a, a, a) (b, b, b, b) where
  type In  (a, a, a, a) = a
  type Out (b, b, b, b) = b
  mapT f (a, b, c, d) = (f a, f b, f c, f d)

{-
data family T2 a :: * -> *
data instance T2 a = (a, a)
data family T3 a :: * -> *
data instance T3 a = (a, a, a)
data family T4 a :: * -> *
data instance T4 a = (a, a, a, a)

instance Functor T2 where
  fmap f (a, b) = (f a, f b)

instance Functor T3 where
  fmap f (a, b, c) = (f a, f b, f c)

instance Functor T4 where
  fmap f (a, b, c, d) = (f a, f b, f c, f d)
-}

-- From numeric-quest
-- | Compositional power of a function,
-- i.e. apply the function n times to a value.
nest :: Int -> (a -> a) -> a -> a
nest 0 _ x = x
nest n f x = f (nest (n-1) f x)

uncurry3 f (a, b, c) = f a b c
uncurry4 f (a, b, c, d) = f a b c d 

debug :: (Show a) => a -> a
debug x = trace (show x) x
