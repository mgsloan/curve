module Main where

import Test.QuickCheck.PolyQC
import qualified Language.Haskell.Interpreter as GHCi
import Data.List (isInfixOf)

ivlts = ["Interval Float",  "(Interval Float,  Interval Float)",
         "Interval Double", "(Interval Double, Interval Double)"]

curves a = [ "Bezier " ++ a
           , "Const "  ++ a
           , "Linear " ++ a
           , "Poly "   ++ a
           , "SBasis " ++ a]

without s = filter (\x -> not $ any (`isInfixOf`x) s)

dcurves = curves "Double"

imports = ModuleImports "Tests" [] ["Data.Curve"] []

doTest t xs = do
  putStrLn $ "\ntesting " ++ t
  print =<< polyQuickCheck imports t xs

main = do
  doTest "prop_union"          ivlts
  doTest "prop_intersect"      ivlts
  doTest "prop_is_finite"      dcurves
  doTest "prop_is_zero"        dcurves
  doTest "prop_compose"        dcurves
  doTest "prop_bounds"         dcurves
  doTest "prop_portion"        $ without ["Bezier", "SBasis"] dcurves
  doTest "prop_portion_bounds" $ without ["Bezier", "SBasis"] dcurves
  doTest "prop_zeroV"          dcurves
  doTest "prop_sum"            $ without ["Bezier"] dcurves
  doTest "prop_scale"          dcurves
  doTest "prop_offset"         dcurves
  doTest "prop_domain_bounds"  dcurves
  doTest "prop_roots"          ["SBasis Double"]
  doTest "prop_to_bezier"      $ without ["SBasis"] dcurves
  doTest "prop_to_poly"        $ without ["Bezier", "SBasis"] dcurves
  doTest "prop_to_sbasis"      dcurves
