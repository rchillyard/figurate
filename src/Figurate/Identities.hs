-- | Famous identities linking triangular numbers to their relatives.
--
-- Each identity is stored as /data/: a name together with a function that
-- checks it for one value of n.  Functions are ordinary values in
-- Haskell, so they can live inside a record like any other field.
module Figurate.Identities
  ( Identity (..)
  , identities
  , firstFailure
  , threeTriangulars
  ) where

import Data.List (find)
import Data.Maybe (listToMaybe)

import Figurate.Family
import Figurate.Triangular

-- | Record syntax names the fields, and also defines accessor functions
-- @identityName :: Identity -> String@ and @holdsFor :: Identity -> (Integer -> Bool)@.
data Identity = Identity
  { identityName :: String
  , holdsFor     :: Integer -> Bool
  }

t :: Integer -> Integer
t = triangular

sq, cube :: Integer -> Integer
sq x = x * x
cube x = x * x * x

-- | Each one is meant to hold for every n >= 1.
--
-- @\\n -> ...@ is a lambda (an anonymous function), and @$@ is
-- low-precedence function application, so we can avoid wrapping the
-- lambda in parentheses.
identities :: [Identity]
identities =
  [ Identity "T(n) + T(n-1) = n²" $ \n ->
      t n + t (n - 1) == sq n
  , Identity "8·T(n) + 1 = (2n+1)²" $ \n ->
      8 * t n + 1 == sq (2 * n + 1)
  , Identity "1³ + 2³ + … + n³ = T(n)²   (Nicomachus)" $ \n ->
      sum (map cube [1 .. n]) == sq (t n)
  , Identity "2·T(n) = n(n+1), the nth pronic number" $ \n ->
      2 * t n == nth Pronic n
  , Identity "T(n)² + T(n+1)² = T((n+1)²)" $ \n ->
      sq (t n) + sq (t (n + 1)) == t (sq (n + 1))
  , Identity "T(1) + … + T(n) = Tet(n)   (tetrahedral)" $ \n ->
      sum (map t [1 .. n]) == nth (Simplex 3) n
  , Identity "1² + … + n² = SqPyr(n)   (square pyramidal)" $ \n ->
      sum (map sq [1 .. n]) == nth (Pyramidal 4) n
  , Identity "Simplex(d, n) = Simplex(d-1, 1) + … + Simplex(d-1, n), d = 2..6" $ \n ->
      and [sum (map (nth (Simplex (d - 1))) [1 .. n]) == nth (Simplex d) n | d <- [2 .. 6]]
  , Identity "P(s, n) = T(n) + (s-3)·T(n-1), s = 3..12   (every polygonal number)" $ \n ->
      all (\s -> nth (Polygonal s) n == t n + (s - 3) * t (n - 1)) [3 .. 12]
  , Identity "Hex(n) = T(2n-1): every hexagonal number is triangular" $ \n ->
      nth (Polygonal 6) n == t (2 * n - 1)
  , Identity "CHex(n) = 6·T(n-1) + 1   (centered hexagonal)" $ \n ->
      nth (Centered 6) n == 6 * t (n - 1) + 1
  , Identity "CHex(1) + … + CHex(n) = n³" $ \n ->
      sum (map (nth (Centered 6)) [1 .. n]) == cube n
  ]

-- | The first n in 1 .. limit for which the identity fails, if any.
firstFailure :: Integer -> Identity -> Maybe Integer
firstFailure limit identity = find (not . holdsFor identity) [1 .. limit]

-- | Gauss's "Eureka" theorem (1796): every natural number is the sum of
-- three triangular numbers (counting 0 as triangular).
--
-- A list comprehension with several generators acts like nested loops,
-- and its conditions act as filters.  We ask for only the first result
-- ('listToMaybe'), and laziness means the search stops as soon as it
-- finds one.
threeTriangulars :: Integer -> Maybe (Integer, Integer, Integer)
threeTriangulars x =
  listToMaybe
    [ (a, b, c)
    | a <- ts
    , b <- takeWhile (\v -> a + v <= x) (dropWhile (< a) ts)
    , let c = x - a - b
    , c >= b
    , isTriangular c
    ]
  where
    ts = takeWhile (<= x) triangulars
