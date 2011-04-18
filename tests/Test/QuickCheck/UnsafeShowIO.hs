-- vim: sw=2: ts=2: expandtab:

-- | crazy hack !!!
-- to force evaluating quickCheck in the hint Haskell interpreter
-- you shouldn't really do this in any other places
module Test.QuickCheck.UnsafeShowIO where

import System.IO.Unsafe

instance Show a => Show (IO a) where
  show = show . unsafePerformIO
