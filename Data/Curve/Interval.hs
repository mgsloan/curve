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
-- Implementation of 1-D intervals.  Rather than implementing an additional
-- class for bounding boxes, we utilize the two-dimensional lifter.
-- Based on Edward Kmett's intervals-0.1.3 package defining
-- Numeric.Interval (later versions got rid of rounding mode stuff)

{-# LANGUAGE Rank2Types, TypeFamilies, TupleSections, DeriveDataTypeable #-} 

module Data.Curve.Interval 
    ( Interval(..)
    , (...)
    , whole
    , empty
    , null
    , singleton
    , elem
    , notElem
    , inf
    , sup
    , singular
    , width
    , intersection
    , hull
    , bisection
    , midpoint
    , magnitude
    , mignitude
    , contains
    , isSubsetOf
    , certainly, (<!), (<=!), (==!), (>=!), (>!)
    , possibly, (<?), (<=?), (==?), (>=?), (>?)
    , idouble 
    , ifloat 
    -- Data.Curve.Interval extensions (added to Numeric.Interval)
    , extend
    , extent
    , expand 
    , mapping
    , localTo
    , wrap
    , clamp
    , fromList
    , unit
    , overlaps
    , overlapsBetween
    ) where

import Prelude hiding (null, elem, notElem)

import Data.Curve.Classes
import Data.Curve.Util (sweep)
import Data.Data

import Algebra.Lattice
import Numeric.Extras
import Numeric.Rounding
import Control.Arrow 
import Data.Function (on)
import Data.List (sort, delete, sortBy)
import Data.Ord (comparing)

data Interval a = I !(Round Down a) !(Round Up a)
  deriving (Typeable)

infix 3 ...

negInfinity :: Fractional a => a
negInfinity = (-1)/0 
{-# INLINE negInfinity #-}

posInfinity :: Fractional a => a
posInfinity = 1/0
{-# INLINE posInfinity #-}

nan :: Fractional a => a 
nan = 0/0

-- | The rule of thumb is you should only use this to construct using values
-- that you took out of the interval. Otherwise, use I, to force rounding
(...) :: a -> a -> Interval a 
a ... b = I (Round a) (Round b)
{-# INLINE (...) #-}

-- | The whole real number line
whole :: Precision a => Interval a 
whole = negInfinity ... posInfinity
{-# INLINE whole #-}

-- | An empty interval
empty :: Precision a => Interval a 
empty = nan ... nan
{-# INLINE empty #-}

-- | negation handles NaN properly
null :: Ord a => Interval a -> Bool
null x = not (inf x <= sup x)
{-# INLINE null #-}

-- | A singleton point
singleton :: a -> Interval a 
singleton a = a ... a
{-# INLINE singleton #-}

-- | The infinumum (lower bound) of an interval
inf :: Interval a -> a
inf (I (Round a) _) = a
{-# INLINE inf #-}

-- | The supremum (upper bound) of an interval
sup :: Interval a -> a
sup (I _ (Round b)) = b
{-# INLINE sup #-}

-- | Is the interval a singleton point? 
-- N.B. This is fairly fragile and likely will not hold after
-- even a few operations that only involve singletons
singular :: Ord a => Interval a -> Bool
singular x = not (null x) && inf x == sup x
{-# INLINE singular #-}

instance Precision a => Eq (Interval a) where
    (==) = (==) `on` midpoint

instance Show a => Show (Interval a) where
    showsPrec n (I (Round a) (Round b)) =   
        showParen (n > 3) $
            showsPrec 3 a .
            showString " ... " . 
            showsPrec 3 b

-- change rounding type
cr :: Round ra a -> Round rb a
cr (Round a) = Round a
{-# INLINE cr #-}

-- | Calculate the width of an interval.
-- N.B. the width of an interval is an interval itself due to rounding
width :: Precision a => Interval a -> Interval a
width (I a b) = I (cr b - a) (b - cr a)
{-# INLINE width #-}

-- | magnitude 
magnitude :: Precision a => Interval a -> a 
magnitude x = (max `on` abs) (inf x) (sup x)
{-# INLINE magnitude #-}

-- | "mignitude"
mignitude :: Precision a => Interval a -> a 
mignitude x = (min `on` abs) (inf x) (sup x)
{-# INLINE mignitude #-}

instance Precision a => Num (Interval a) where
    I a b + I a' b' = I (a + a') (b + b')
    I a b - I a' b' = I (a - cr b') (b - cr a')
    I a b * I a' b' = minimum [a * a',a * cr b',cr b * a',cr b * cr b'] 
                      `I` 
                      maximum [cr a * cr a',cr a * b',b * cr a',b * b']
    abs x@(I a b) 
        | a >= 0    = x 
        | b <= 0    = negate x
        | otherwise = max (- a) (cr b) `I` b

    signum (I a b)  = signum a `I` signum b

    fromInteger i   = fromInteger i `I` fromInteger i

-- | Bisect an interval at its midpoint.
bisection :: Precision a => Interval a -> (Interval a, Interval a)
bisection (I a b) = (I a (cr a + (b - cr a) / 2), I (a + (cr b - a) / 2) b)
{-# INLINE bisection #-}

-- | Nearest point to the midpoint of the interval.
midpoint :: Precision a => Interval a -> a
midpoint x = inf x + (sup x - inf x) / 2
{-# INLINE midpoint #-}

elem :: Precision a => a -> Interval a -> Bool
elem x xs = x >= inf xs && x <= sup xs
{-# INLINE elem #-}

notElem :: Precision a => a -> Interval a -> Bool
notElem x xs = not (elem x xs)
{-# INLINE notElem #-}

-- | This means that realToFrac will use the midpoint
instance Precision a => Real (Interval a) where
    toRational x = toRational (midpoint x)

-- @'divNonZero' X Y@ assumes @0 `'notElem'` Y@
divNonZero :: Precision a => Interval a -> Interval a -> Interval a 
divNonZero (I a b) (I a' b') = 
    minimum [   a /    a',    a / cr b', cr b /    a', cr b / cr b'] 
    `I`
    maximum [cr a / cr a', cr a /    b',    b / cr a',    b /    b']

-- @'divPositive' X y@ assumes y > 0, and divides @X@ by [0 ... y]
divPositive :: Precision a => Interval a -> a -> Interval a 
divPositive x@(I a b) y
    | a == 0 && b == 0 = x
    | b < 0 || isNegativeZero b = negInfinity `I` ( b / up y)
    | a < 0 = whole 
 --  isNegativeZero a = whole
    | otherwise = (a / down y) `I` posInfinity

-- divNegative assumes y < 0 and divides the interval @X@ by [y ... 0]
divNegative :: Precision a => Interval a -> a -> Interval a
divNegative x@(I a b) y
    | a == 0 && b == 0 = - x -- flips negative zeros
    | b < 0 || isNegativeZero b = (cr b / down y) `I` posInfinity
    | a < 0 = whole
 --  isNegativeZero a = whole
    | otherwise = negInfinity `I` (cr a / up y)

divZero :: Precision a => Interval a -> Interval a
divZero x | inf x == 0 && sup x == 0 = x
          | otherwise = whole

instance Precision a => Fractional (Interval a) where
    -- TODO: check isNegativeZero properly
    x / y
        | 0 `notElem` y = divNonZero x y 
        | iz && sz  = empty -- division by 0
        | iz        = divPositive x (inf y)
        |       sz  = divNegative x (sup y)
        | otherwise = divZero x
        where 
            iz = inf y == 0
            sz = sup y == 0
    recip (I a b)   = on min recip a (cr b) `I` on max recip (cr a) b
    fromRational r  = fromRational r `I` fromRational r

instance Precision a => RealFrac (Interval a) where
    properFraction x = (b, x - fromIntegral b)
        where 
            b = truncate (midpoint x)
    ceiling x = ceiling (sup x)
    floor x = floor (inf x)
    round x = round (midpoint x)
    truncate x = truncate (midpoint x)

instance Precision a => Floating (Interval a) where
    pi = pi `I` pi 
    exp = increasing exp
    log (I a b) = (if a > 0 then log a else negInfinity) `I` log b
    cos x 
        | null x = empty
        | inf (width t) >= inf pi = (-1) ... 1
        | tl >= cr pih  = - cos (t - pi)
        | th <= cr pil  = cos (cr th) `I` cos (cr tl)
        | th <= cr pi2l = (-1) `I` cos (cr (min (pi2l - cr th) tl))
        | otherwise  = (-1) ... 1
        where 
            I pil pih = pi
            pi2@(I pi2l _) = pi * 2
            t@(I tl th) = x `fmod` pi2
    sin x 
        | null x = empty
        | otherwise = cos (x - pi / 2)
    tan x 
        | null x = empty
        | inf t' <= -hpil || sup t' >= hpil = whole
        | otherwise = increasing tan x
        where
            t = x `fmod` pi 
            t' | inf t >= hpil = t - pi
               | otherwise = t
            hpil = inf (pi / 2)
    asin x@(I a b)
        | null x || b < -1 || a > 1 = empty
        | otherwise = 
            (if a <= - 1 then - cr hpis else asin a)
            `I`
            (if b >= 1 then hpis else asin b)
        where
            I _ hpis = pi / 2
    acos x@(I a b)
        | null x || b < -1 || a > 1 = empty
        | otherwise = 
            (if b >= 1 then 0 else acos (cr b))
            `I`
            (if a < -1 then pis else acos (cr a))
        where
            I _ pis = pi
    atan = increasing atan
    sinh = increasing sinh
    cosh x@(I a b)
        | null x = empty
        | b < 0  = decreasing cosh x
        | a >= 0 = increasing cosh x
        | otherwise  = I 0 $ cosh $ if - cr a > b
                                    then cr a 
                                    else b
    tanh = increasing tanh
    asinh = increasing asinh
    acosh x@(I a b)
        | null x || b < 1 = empty -- acosh is only defined on [1..1/0)
        | otherwise = I lo $ acosh b
        where lo | a <= 1 = 0 
                 | otherwise = acosh a
    atanh x@(I a b)
        | null x || b < -1 || a > 1 = empty
        | otherwise =
                (if a <= - 1 then negInfinity else atanh a)
                `I`
                (if b >= 1 then posInfinity else atanh b)
    
-- | lift a monotone increasing function over a given interval 
increasing :: Precision a => 
         (forall d. Rounding d => Round d a -> Round d a) -> 
         Interval a -> Interval a
increasing f (I a b) = I (f a) (f b)

-- | lift a monotone increasing function over a given interval 
decreasing :: Precision a => 
         (forall d. Rounding d => Round d a -> Round d a) -> 
         Interval a -> Interval a
decreasing f (I a b) = I (f (cr b)) (f (cr a))


-- | We have to play some semantic games to make these methods make sense.
-- Most compute with the midpoint of the interval.
instance Precision a => RealFloat (Interval a) where
    floatRadix = floatRadix . midpoint
    floatDigits = floatDigits . midpoint
    floatRange = floatRange . midpoint
    decodeFloat = decodeFloat . midpoint
    encodeFloat m e = singleton (encodeFloat m e)
    exponent = exponent . midpoint
    significand x = min a b ... max a b
        where
            (_ ,em) = decodeFloat (midpoint x)
            (mi,ei) = decodeFloat (inf x)
            (ms,es) = decodeFloat (sup x)
            a = encodeFloat mi (ei - em - floatDigits x) 
            b = encodeFloat ms (es - em - floatDigits x)
    scaleFloat n x = scaleFloat n (inf x) ... scaleFloat n (sup x)
    isNaN x = isNaN (inf x) || isNaN (sup x)
    isInfinite x = isInfinite (inf x) || isInfinite (sup x)
    isDenormalized x = isDenormalized (inf x) || isDenormalized (sup x)
    -- contains negative zero
    isNegativeZero x = not (inf x > 0) 
                    && not (sup x < 0)
                    && (  (sup x == 0 && (inf x < 0 || isNegativeZero (inf x)))
                       || (inf x == 0 && isNegativeZero (inf x)) 
                       || (inf x < 0 && sup x >= 0))
    isIEEE x = isIEEE (inf x) && isIEEE (sup x)
    atan2 = error "unimplemented"

-- TODO: (^), (^^) to give tighter bounds
        
-- | Calculate the intersection of two intervals.
intersection :: Precision a => Interval a -> Interval a -> Interval a
intersection x@(I a b) y@(I a' b')
    | x /=! y = empty
    | otherwise = I (max a a') (min b b')

-- | Calculate the convex hull of two intervals
hull :: Ord a => Interval a -> Interval a -> Interval a
hull x@(I a b) y@(I a' b') 
    | null x = y
    | null y = x
    | otherwise = I (min a a') (max b b')
    
instance Precision a => RealExtras (Interval a) where
    type C (Interval a) = C a
    -- output always lies within the interval y if y >=! 0
    fmod x y | null y = empty 
             | otherwise = r -- `intersection` bounds
        where 
            n :: Integer
            n = floor (inf x / if inf x < 0 then inf y else sup y)
            r = x - fromIntegral n * y 
            -- bounds | inf y >= 0 = y
            --        | otherwise = y `hull` negate y
    expm1 = increasing expm1
    log1p (I a b) = (if a > (-1) then log1p a else negInfinity) `I` log1p b
    hypot x y = hypot a a' `I` hypot b b'
        where
            I a b = abs x
            I a' b' = abs y
    cbrt = increasing cbrt
    erf = increasing erf

-- | For all @x@ in @X@, @y@ in @Y@. @x '<' y@
(<!)  :: Ord a => Interval a -> Interval a -> Bool
x <! y = sup x < inf y
{-# INLINE (<!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<=' y@
(<=!) :: Ord a => Interval a -> Interval a -> Bool
x <=! y = sup x <= inf y
{-# INLINE (<=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '==' y@
(==!) :: Eq a => Interval a -> Interval a -> Bool
x ==! y = sup x == inf y && inf x == sup y
{-# INLINE (==!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '/=' y@
(/=!) :: Ord a => Interval a -> Interval a -> Bool
x /=! y = sup x < inf y || inf x > sup y
{-# INLINE (/=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>' y@
(>!)  :: Ord a => Interval a -> Interval a -> Bool
x >! y = inf x > sup y
{-# INLINE (>!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>=' y@
(>=!) :: Ord a => Interval a -> Interval a -> Bool
x >=! y = inf x >= sup y
{-# INLINE (>=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x `op` y@
certainly :: Ord a => (forall b. Ord b => b -> b -> Bool) -> Interval a -> Interval a -> Bool
certainly cmp l r 
    | lt && eq && gt = True
    | lt && eq       = l <=! r
    | lt &&       gt = l /=! r
    | lt             = l <! r 
    |       eq && gt = l >=! r 
    |       eq       = l ==! r
    |             gt = l >! r
    | otherwise      = False
    where 
        lt = cmp LT EQ
        eq = cmp EQ EQ
        gt = cmp GT EQ
{-# INLINE certainly #-}

contains :: Ord a => Interval a -> Interval a -> Bool
contains x y = null y 
            || (not (null x) && inf x <= inf y && sup y <= sup x)
{-# INLINE contains #-}

isSubsetOf :: Ord a => Interval a -> Interval a -> Bool
isSubsetOf = flip contains

-- | Comparisons are made on the midpoint
instance Precision a => Ord (Interval a) where
    compare = compare `on` midpoint
    max (I a b) (I a' b') = I (max a a') (max b b')
    min (I a b) (I a' b') = I (min a a') (min b b')

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<' y@?
(<?) :: Ord a => Interval a -> Interval a -> Bool
x <? y = inf x < sup y
{-# INLINE (<?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<=' y@?
(<=?) :: Ord a => Interval a -> Interval a -> Bool
x <=? y = inf x <= sup y
{-# INLINE (<=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '==' y@?
(==?) :: Ord a => Interval a -> Interval a -> Bool
x ==? y = inf x <= sup y && sup x >= inf y
{-# INLINE (==?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '/=' y@?
(/=?) :: Eq a => Interval a -> Interval a -> Bool
x /=? y = inf x /= sup y || sup x /= inf y
{-# INLINE (/=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>' y@?
(>?) :: Ord a => Interval a -> Interval a -> Bool
x >? y = sup x > inf y
{-# INLINE (>?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>=' y@?
(>=?) :: Ord a => Interval a -> Interval a -> Bool
x >=? y = sup x >= inf y
{-# INLINE (>=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x `op` y@?
possibly :: Ord a => (forall b. Ord b => b -> b -> Bool) -> Interval a -> Interval a -> Bool
possibly cmp l r 
    | lt && eq && gt = True
    | lt && eq       = l <=? r
    | lt &&       gt = l /=? r
    | lt             = l <? r 
    |       eq && gt = l >=? r 
    |       eq       = l ==? r
    |             gt = l >? r
    | otherwise      = False
    where 
        lt = cmp LT EQ
        eq = cmp EQ EQ
        gt = cmp GT EQ
{-# INLINE possibly #-}

-- | Function used to specify double precision intervals.  Only useful to
-- introduce the type constraint - definition is just 'id'.
idouble :: Interval Double -> Interval Double
idouble = id

-- | Function used to specify single precision intervals.  Only useful to
-- introduce the type constraint - definition is just 'id'.
ifloat :: Interval Float -> Interval Float
ifloat = id

-- Michael Sloan's additions

--TODO: look into these / add tests
-- Bugs:
-- sin 1 :: Interval Double

instance (Precision a) => Predicate (Interval a) where
    type Element (Interval a) = a
    element = elem

instance (Precision a) => JoinSemiLattice (Interval a) where 
    join = hull
instance (Precision a) => MeetSemiLattice (Interval a) where
    meet = intersection

-- | Extends the interval, if necessary, in order to contain the provided value.
extend :: (Precision a) => Interval a -> a -> Interval a
extend (I (Round f) (Round t)) v 
  | v < f     = v ... t
  | v > t     = f ... v
  | otherwise = f ... t

-- | Yields the width of the interval.  This is the midpoint of the value
-- returned by 'width'
extent :: (Precision a) => Interval a -> a
extent = midpoint . width

-- | Expands the interval, in both directions, by the provided amount.
expand :: (Precision a) => a -> Interval a -> Interval a
expand v (I f t) = I (f - down v) (t + up v)

-- | Given two intervals, this yields a linear mapping from the first to
-- the second.  In other words (inf a) is mapped to (inf b), etc.
mapping :: (Precision a) => Interval a -> Interval a -> a -> a
mapping fr@(I (Round f1) _) to@(I (Round f2) _) a = (a - f1) * ratio + f2
  where ratio = extent to / extent fr

-- | Given two intervals, this yields a linear mapping from the first to
-- the second.  In other words (inf a) is mapped to (inf b), etc.
-- This function is distinct from mapping in that it handles rounding in
-- the appropriate direction
rmapping :: (Precision a, Rounding dir) => Interval a -> Interval a -> Round dir a -> Round dir a
rmapping fr@(I f _) to@(I t _) a = (a - cr f) * ratio + cr t
  where ratio = Round . inf $ width to / width fr

unit :: (Precision a) => Interval a
unit = I (Round 0) (Round 1)

-- | The first interval is transformed into the coordinate system of the
-- second, arrived at considering its 
localTo :: (Precision a) => Interval a -> Interval a -> Interval a
localTo (I f t) outer = I (g1 f) (g2 t)
  where g1 = rmapping outer unit
        g2 = rmapping outer unit

-- | Wraps values into the interval,
wrap :: (RealFrac a, Precision a) => Interval a -> a -> a
wrap i = mapping unit i
       . (\x -> (if x < 0 then (1+) else id) $ snd (properFraction x))
       . mapping i unit

-- | Values which fall outside the interval are clamped to the extrema.  In
-- other words, values less than the infinum are set to the infimum, and
-- values greater than the supremum are set to the supremum.
clamp :: (Precision a) => Interval a -> a -> a
clamp (I (Round f) (Round t)) x 
  | x < f = f
  | x > t = t
  | otherwise = x

-- | Given a list, yields an Interval which contains all elements.
fromList :: (Precision a) => [a] -> Interval a
fromList xs = head sorted ... last sorted
    where sorted = sort xs

-- Helper for overlaps and overlapsBetween.
events :: (Interval t1, t) -> [(t, Bool, t1, t1)]
events ((I (Round f) (Round t)), x) = [(x, False, f, t), (x, True, f, t)]

eventMetric :: (t, Bool, t1, t1) -> (t1, Bool)
eventMetric (_, b, f, t) = (if b then t else f, b)

--TODO: use more efficient data structures for context storage.

-- | Given a list of intervals associated with values, returns pairs of
-- values which overlap.
overlaps :: (Precision a, Ord b) => [(Interval a, b)] -> [(Interval a, b, b)]
overlaps = concat . sweep handler [] . sortBy (comparing eventMetric) . concatMap events
  where handler (x, False, f, t) = ((x, f, t):) &&& map report
          where report (y, fy, ty) | x < y     = (inter, x, y)
                                   | otherwise = (inter, y, x)
                  where inter = meet (f ... t) (fy ... ty)
        handler (x, True, f, t) = delete (x, f, t) &&& const []

-- | Detects overlaps between intervals in two different lists.
-- Given two lists of intervals associated with values, returns pairs of 
-- values.  One is from the first set, one is from the 
overlapsBetween :: (Precision a, Eq b) => 
    [(Interval a, b)] -> [(Interval a, b)] -> [(Interval a, b, b)] 
overlapsBetween xs ys = concat . sweep handler ([], []) $ 
      sortBy (comparing $ eventMetric . snd) $
      concatMap (map (False,) . events) xs ++ 
      concatMap (map (True,)  . events) ys 
    where handler (b, (x, True, f, t)) 
              | b         = first  ((x, f, t):) &&& (map report . snd)
              | otherwise = second ((x, f, t):) &&& (map report . fst)
            where report (y, fy, ty) | b         = (inter, x, y)
                                     | otherwise = (inter, y, x)
                    where inter = meet (f ... t) (fy ... ty)
          handler (b, (x, False, f, t))
              | b         = (first  $ delete (x, f, t)) &&& const []
              | otherwise = (second $ delete (x, f, t)) &&& const []
