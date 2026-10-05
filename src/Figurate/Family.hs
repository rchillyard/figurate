-- | Families of figurate numbers: triangular numbers and their relatives.
--
-- The central idea here is an /algebraic data type/.  'Family' says that a
-- family of figurate numbers is exactly one of five kinds, and functions
-- over it are written by pattern matching on which kind it is.  Compare a
-- sealed trait with case classes in Scala, or a sealed interface with
-- records in Java.
module Figurate.Family
  ( Family (..)
  , nth
  , members
  , familyName
  , indexIn
  , namedFamilies
  , parseFamily
  , binomial
  ) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | A family of figurate numbers.  Each alternative is a /constructor/,
-- and a constructor may carry fields.  @Polygonal 3@ is the triangular
-- numbers, @Polygonal 4@ the squares, and so on.
--
-- @deriving (Eq, Show)@ asks the compiler to write equality and a
-- printable representation for us.
data Family
  = Polygonal Integer   -- ^ s-gonal numbers: 1, s, 3s-3, ...   (s >= 3)
  | Centered Integer    -- ^ centered s-gonal numbers: 1, s+1, 3s+1, ...
  | Pronic              -- ^ n(n+1): the "oblong" numbers 2, 6, 12, ...
  | Pyramidal Integer   -- ^ s-gonal pyramidal: running sums of s-gonal numbers
  | Simplex Integer     -- ^ triangular numbers in d dimensions
  deriving (Eq, Show)

-- | The nth member of a family, counting from n = 1.
--
-- One equation per constructor.  If we forgot one, @-Wall@ would warn
-- that the patterns are non-exhaustive.
nth :: Family -> Integer -> Integer
nth (Polygonal s) n = ((s - 2) * n * n - (s - 4) * n) `div` 2
nth (Centered s)  n = s * n * (n - 1) `div` 2 + 1
nth Pronic        n = n * (n + 1)
nth (Pyramidal s) n = n * (n + 1) * ((s - 2) * n - (s - 5)) `div` 6
nth (Simplex d)   n = binomial (n + d - 1) d

-- | Every member of a family, as an infinite lazy list.
members :: Family -> [Integer]
members fam = map (nth fam) [1 ..]

-- | The binomial coefficient "n choose k".  Simplex numbers are the
-- diagonals of Pascal's triangle: naturals (d = 1), triangular (d = 2),
-- tetrahedral (d = 3), pentatope (d = 4), ...
binomial :: Integer -> Integer -> Integer
binomial n k
  | k < 0 || k > n = 0
  | otherwise      = product [n - k + 1 .. n] `div` product [1 .. k]

-- | A human-readable name for a family.
familyName :: Family -> String
familyName (Polygonal s) = polygonName s
familyName (Centered s)  = "centered " ++ polygonName s
familyName Pronic        = "pronic"
familyName (Pyramidal s) = polygonName s ++ " pyramidal"
familyName (Simplex d)   =
  case d of
    1 -> "natural"
    2 -> "triangular"
    3 -> "tetrahedral"
    4 -> "pentatope"
    _ -> show d ++ "-simplex"

-- | 'lookup' searches a list of (key, value) pairs and returns a Maybe;
-- 'fromMaybe' supplies a default for the Nothing case.
polygonName :: Integer -> String
polygonName s = fromMaybe (show s ++ "-gonal") (lookup s names)
  where
    names =
      [ (3, "triangular"), (4, "square"), (5, "pentagonal"), (6, "hexagonal")
      , (7, "heptagonal"), (8, "octagonal"), (9, "nonagonal"), (10, "decagonal")
      , (12, "dodecagonal")
      ]

-- | If x is in the family, return @Just n@ where x is its nth member.
--
-- This searches an /infinite/ list, and terminates only because
-- 'takeWhile' stops as soon as the members exceed x (they are increasing).
-- We pair each member with its index using @zip ... [1 ..]@.
indexIn :: Family -> Integer -> Maybe Integer
indexIn fam x = lookup x (takeWhile ((<= x) . fst) (zip (members fam) [1 ..]))

-- | The families the command-line program knows by name.
namedFamilies :: [(String, Family)]
namedFamilies =
  [ ("natural",              Simplex 1)
  , ("triangular",           Polygonal 3)
  , ("square",               Polygonal 4)
  , ("pentagonal",           Polygonal 5)
  , ("hexagonal",            Polygonal 6)
  , ("heptagonal",           Polygonal 7)
  , ("octagonal",            Polygonal 8)
  , ("pronic",               Pronic)
  , ("centered-triangular",  Centered 3)
  , ("centered-square",      Centered 4)
  , ("centered-hexagonal",   Centered 6)
  , ("tetrahedral",          Simplex 3)
  , ("square-pyramidal",     Pyramidal 4)
  , ("pentatope",            Simplex 4)
  ]

-- | Parse a family name, such as "triangular", or a parameterised form
-- such as "polygonal:11", "centered:5", "pyramidal:6" or "simplex:5".
--
-- 'break' splits "polygonal:11" into ("polygonal", ":11"), and the case
-- expression pattern-matches on that pair, including the ':' character.
--
-- @Polygonal <$> m@ applies the constructor /inside/ the Maybe: it gives
-- @Just (Polygonal k)@ if m is @Just k@, and Nothing if m is Nothing.
-- ('<$>' is 'fmap', the map operation of a Functor.)
parseFamily :: String -> Maybe Family
parseFamily str =
  case break (== ':') str of
    ("polygonal", ':' : k) -> Polygonal <$> atLeast 3 k
    ("centered",  ':' : k) -> Centered  <$> atLeast 3 k
    ("pyramidal", ':' : k) -> Pyramidal <$> atLeast 3 k
    ("simplex",   ':' : k) -> Simplex   <$> atLeast 1 k
    _                      -> lookup str namedFamilies
  where
    atLeast :: Integer -> String -> Maybe Integer
    atLeast lo s =
      case readMaybe s of
        Just v | v >= lo -> Just v
        _                -> Nothing
