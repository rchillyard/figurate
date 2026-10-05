-- | Property-based tests with QuickCheck.
--
-- Instead of writing individual test cases, we state properties that
-- should hold for /all/ inputs, and QuickCheck tries each one on 100
-- random inputs.  If a property fails, QuickCheck "shrinks" the failing
-- input to the smallest counterexample it can find.
module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.QuickCheck

import Figurate.Family
import Figurate.Identities
import Figurate.Picture
import Figurate.Triangular

main :: IO ()
main = do
  results <- mapM check properties
  unless (and results) exitFailure
  where
    check (name, prop) = do
      putStr (name ++ ": ")
      isSuccess <$> quickCheckResult prop

-- | Wrapping each property with 'property' gives them all the same type,
-- so they can go into one list.
--
-- @\\(NonNegative n) -> ...@ pattern-matches on QuickCheck's 'NonNegative'
-- wrapper, which tells QuickCheck to generate only n >= 0.
properties :: [(String, Property)]
properties =
  [ ("recursive T(n) agrees with closed form", property $ \(NonNegative n) ->
      triRecursive n == triClosedForm n)
  , ("accumulating T(n) agrees with closed form", property $ \(NonNegative n) ->
      triAccumulating n == triClosedForm n)
  , ("fold T(n) agrees with closed form", property $ \(NonNegative n) ->
      triFold n == triClosedForm n)
  , ("scanl sequence agrees with closed form", property $ \(NonNegative n) ->
      triangulars !! fromInteger n == triClosedForm n)
  , ("knot-tied sequence agrees with scanl", property $ \(NonNegative n) ->
      triangularsKnot !! n == triangulars !! n)
  , ("isqrt is the floor of the square root", forAll (choose (0, 10 ^ (40 :: Int))) $ \m ->
      let r = isqrt m in r * r <= m && m < (r + 1) * (r + 1))
  , ("triangularRoot inverts triangular", property $ \(NonNegative n) ->
      triangularRoot (triangular n) == Just n)
  , ("isTriangular agrees with brute-force search", property $ \(NonNegative x) ->
      isTriangular x == (x `elem` takeWhile (<= x) triangulars))
  , ("Simplex 2 is the triangular numbers", property $ \(NonNegative n) ->
      nth (Simplex 2) n == triangular n)
  , ("Polygonal 3 is the triangular numbers", property $ \(NonNegative n) ->
      nth (Polygonal 3) n == triangular n)
  , ("tetrahedral = triangular pyramidal", property $ \(NonNegative n) ->
      nth (Simplex 3) n == nth (Pyramidal 3) n)
  , ("indexIn inverts nth", forAll genFamily $ \fam -> forAll (choose (1, 50)) $ \n ->
      indexIn fam (nth fam n) == Just n)
  , ("every picture has the right number of dots", forAll genFamily $ \fam -> forAll (choose (1, 15)) $ \n ->
      case picture fam n of
        Nothing  -> True
        Just pic -> dotCount pic == nth fam (toInteger n))
  , ("Gauss's Eureka theorem", forAll (choose (0, 5000)) $ \x ->
      case threeTriangulars x of
        Just (a, b, c) -> a + b + c == x && all isTriangular [a, b, c]
        Nothing        -> False)
  ]
  ++ [ ("identity " ++ identityName i, property $ \(Positive n) -> holdsFor i n)
     | i <- identities
     ]

-- | A generator that picks one of the named families at random.
genFamily :: Gen Family
genFamily = elements (map snd namedFamilies)
