-- vim: sw=2: ts=2: expandtab:

{- |
> import Test.QuickCheck.PolyQC
> import Prop -- the module that defiens the properties p0, p1, p2, p3, p4
>  -- p0 x = x == x
>  -- p1 x y z = x + (y + z) == (x + y) + z
>  -- p2 x y = x + y == y + x
>  -- p3 x = x == negate (negate x)
>  -- p4 p = (fst p, snd p) == p
> main = do putStrLn "testing p0 ======================================="
>           print =<< polyQuickCheck' "Prop" "p0" ["Bool","Int","Double"]
>           putStrLn "testing p1 ======================================="
>           print =<< polyQuickCheck' "Prop" "p1" ["Bool","Int","Double"]
>           putStrLn "testing p2 ======================================="
>           print =<< polyQuickCheck' "Prop" "p2" ["Bool","Int","Double"]
>           putStrLn "testing p3 ======================================="
>           print =<< polyQuickCheck' "Prop" "p3" ["Bool","Int","Double"]
>           putStrLn "testing p4 ======================================="
>           print =<< polyQuickCheck' "Prop" "p4" ["Bool","Int","Double"]
>           return ()

the result of running this is

> > :t p0
> p0 :: (Eq a) => a -> Bool
> > :t p1
> p1 :: (Num a) => a -> a -> a -> Bool
> > :t p2
> p2 :: (Num a) => a -> a -> Bool
> > :t p3
> p3 :: (Num a) => a -> Bool
> > :t p4
> p4 :: (Eq a, Eq b) => (a, b) -> Bool
> > main
> testing p0 =======================================
> Right ["(\"(Eq Bool) => Bool -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Int) => Int -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Double) => Double -> Bool\",+++ OK, passed 100 tests.
> ())"]
> testing p1 =======================================
> Right ["(\"(Num Int) => Int -> Int -> Int -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Num Double) => Double -> Double -> Double -> Bool\",*** Failed! Falsifiable (after 9 tests and 2 shrinks):    
> 4.0
> -26.0
> 8.777291602197652
> ())"]
> testing p2 =======================================
> Right ["(\"(Num Int) => Int -> Int -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Num Double) => Double -> Double -> Bool\",+++ OK, passed 100 tests.
> ())"]
> testing p3 =======================================
> Right ["(\"(Num Int) => Int -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Num Double) => Double -> Bool\",+++ OK, passed 100 tests.
> ())"]
> testing p4 =======================================
> Right ["(\"(Eq Bool, Eq Bool) => (Bool, Bool) -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Bool, Eq Int) => (Bool, Int) -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Bool, Eq Double) => (Bool, Double) -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Int, Eq Bool) => (Int, Bool) -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Int, Eq Int) => (Int, Int) -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Int, Eq Double) => (Int, Double) -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Double, Eq Bool) => (Double, Bool) -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Double, Eq Int) => (Double, Int) -> Bool\",+++ OK, passed 100 tests.
> ())","(\"(Eq Double, Eq Double) => (Double, Double) -> Bool\",+++ OK, passed 100 tests.
> ())"]

 -}
module Test.QuickCheck.PolyQC
  ( ModuleImports( .. )
  , withModule
  , polyQuickCheck, polyQuickCheck'
  , polyQuickCheckResult, polyQuickCheckResult'
  ) where

import Array
import Char
import List
import Monad
import Control.Monad.CatchIO
import Text.Regex (subRegex)
import Text.Regex.TDFA
import qualified Language.Haskell.Interpreter as GHCi

tyVarsFromString :: String -> [String]
tyVarsFromString = nub . map fst . concatMap elems .
  matchAllText (makeRegex "\\<[[:lower:]][[:alnum:]]*\\>" :: Regex)

subRegexString :: (String, String) -> String -> String
subRegexString = uncurry $ flip . subRegex . makeRegex

paren s = "("++s++")" -- assuming no trailing comments like this line
-- To do this really seriously we need extra work but for our purpose it's fine
-- See more information here for paren function implemntation
-- http://hackage.haskell.org/packages/archive/hint/0.3.1.0/doc/html/src/Hint-Eval.html#eval

-- all substitutions for the free tyvars xs in the small world of types ts
substitutions xs ts = map (zip xs) $
  foldr (liftM2 mplus) (return mzero) [liftM return ts | _ <- xs]
-- alternative definition is
-- subb xs ys = foldr (\x rests -> [(x,y):rest | y<-ys, rest<-rests]) [[]] xs

{-
typeOfViaGHCi m e = GHCi.runInterpreter $ do
  GHCi.loadModules [m]
  -- GHCi.getModuleExports "Prop"
  -- GHCi.set [GHCi.installedModulesInScope GHCi.:= True]
  GHCi.setTopLevelModules [m]
  -- GHCi.getLoadedModules
  -- GHCi.setImports ["Prelude","Data.Map","Prop"]
  GHCi.setImports [m]
  GHCi.set [ GHCi.languageExtensions GHCi.:= (GHCi.FlexibleContexts :
                                              GHCi.glasgowExtensions) ]
  GHCi.typeOf e
-}

-- | data type for modules to import in the hint Haskell interpreter
data ModuleImports =
     ModuleImports
     { top :: String        -- ^ the module where properties are defined
     , locals :: [String]   -- ^ extra local modules to load
     , packages :: [String] -- ^ extra package modules to import
     , flags :: [GHCi.Extension] -- ^ extra flags to use
     }
   deriving (Eq,Show,Read)

withModule :: String -> ModuleImports
withModule m = ModuleImports { top = m, locals = [], packages = [], flags = []}

polyQuickCheckTemplate qcFun
                       (ModuleImports {top=m,locals=lms,packages=pms,flags=fs})
                       e
                       ts = GHCi.runInterpreter $ do
  GHCi.loadModules $ nub $ [m,"Test.QuickCheck.UnsafeShowIO"] ++ lms
  GHCi.setTopLevelModules [m]
  GHCi.setImports $ nub $ [m,"Monad","Test.QuickCheck"] ++ pms
  GHCi.set [ GHCi.languageExtensions GHCi.:= (GHCi.FlexibleContexts :
                                              GHCi.glasgowExtensions ++ fs) ]
  ty <- GHCi.typeOf e
  let allsubs = substitutions ["\\<"++s++"\\>" | s <- tyVarsFromString ty ]
                              [if any isSpace t then paren t else t | t<-ts]
      monotys = [foldr (.) id (map subRegexString subs) $ ty | subs <- allsubs]
  tys' <- filterM (\t -> GHCi.typeChecks $ e++" :: "++t) monotys
  -- mapM GHCi.eval
  --      ["liftM2 (,) (return "++show t++") " ++
  --       "Test.QuickCheck."++qcFun++" ("++e++" :: "++t++")" | t <- tys']
  mapM GHCi.eval
-- "putStr "++show t++" >>
       ["putStr [' '] >> " ++
        "Test.QuickCheck."++qcFun++" ("++e++" :: "++t++")" | t <- tys']


polyQuickCheck
  :: (Control.Monad.CatchIO.MonadCatchIO m, Functor m) =>
     ModuleImports
     -> String
     -> [[Char]]
     -> m (Either GHCi.InterpreterError [String])
polyQuickCheck mi = polyQuickCheckTemplate "quickCheck" mi

polyQuickCheck'
  :: (Control.Monad.CatchIO.MonadCatchIO m, Functor m) =>
     String
     -> String
     -> [[Char]]
     -> m (Either GHCi.InterpreterError [String])
polyQuickCheck' m = polyQuickCheck (withModule m)

-- may not be very meaningful unless we can get the result value back ...
polyQuickCheckResult mi = polyQuickCheckTemplate "quickCheckResult" mi
polyQuickCheckResult' m = polyQuickCheckResult (withModule m)

-- TODO polyQuickCheckWith and polyQuickCheckWithResult which need Args

