-- | ASCII-art pictures of figurate numbers.
--
-- A picture is just a list of lines.  A @type@ declaration gives an
-- existing type a new name; it doesn't create a new type.
module Figurate.Picture
  ( Picture
  , picture
  , beside
  , gnomonProof
  , pronicProof
  , dotCount
  ) where

import Data.List (dropWhileEnd, intersperse)

import Figurate.Family (Family (..))

type Picture = [String]

-- | Draw the nth member of a family, if we know how to draw it.
--
-- Patterns can look inside constructors, and can match literal values:
-- @Polygonal 3@ matches only the triangular numbers.  The final @_@
-- matches anything else.
picture :: Family -> Int -> Maybe Picture
picture fam n
  | n < 1     = Nothing
  | otherwise =
      case fam of
        Simplex 1   -> Just [row n]
        Simplex 2   -> Just (triangle n)
        Polygonal 3 -> Just (triangle n)
        Polygonal 4 -> Just (square n)
        Polygonal 5 -> Just (house n)
        Pronic      -> Just (rectangle n (n + 1))
        Centered 4  -> Just (diamond n)
        Centered 6  -> Just (hexagon n)
        Simplex 3   -> Just (layers triangle)
        Pyramidal 3 -> Just (layers triangle)
        Pyramidal 4 -> Just (layers square)
        _           -> Nothing
  where
    -- A 3-D shape drawn as its layers, side by side.  'layers' takes
    -- a function (a 2-D shape) as its argument.
    layers shape = beside (map shape [1 .. n])

-- | k copies of a character, separated by spaces: dots 'o' 3 == "o o o"
dots :: Char -> Int -> String
dots c k = intersperse ' ' (replicate k c)

row :: Int -> String
row = dots '*'

-- | The list comprehension reads like set notation:
-- "the list of (indent ++ row k), for each k drawn from [1 .. n]".
triangle :: Int -> Picture
triangle n = [replicate (n - k) ' ' ++ row k | k <- [1 .. n]]

rectangle :: Int -> Int -> Picture
rectangle rows cols = replicate rows (row cols)

square :: Int -> Picture
square n = rectangle n n

-- | Centered square numbers: rows of 1, 3, .., 2n-1, .., 3, 1 dots, which
-- add up to n² + (n-1)².  (Rows of 1, 2, .., n, .., 2, 1 would look
-- similar but add up to only n²: the test suite caught that bug.)
diamond :: Int -> Picture
diamond n =
  [ replicate (2 * n - 1 - k) ' ' ++ row k
  | k <- [1, 3 .. 2 * n - 1] ++ [2 * n - 3, 2 * n - 5 .. 1]
  ]

-- | Centered hexagonal numbers: rows of n, n+1, .., 2n-1, .., n+1, n dots.
hexagon :: Int -> Picture
hexagon n =
  [ replicate (2 * n - 1 - k) ' ' ++ row k
  | k <- [n .. 2 * n - 1] ++ [2 * n - 2, 2 * n - 3 .. n]
  ]

-- | A pentagonal number is a square with a triangular roof:
-- P5(n) = n² + T(n-1).  The roof is drawn with 'o' so you can see it.
house :: Int -> Picture
house n = [replicate (n - k) ' ' ++ dots 'o' k | k <- [1 .. n - 1]] ++ square n

-- | Put pictures side by side, aligned along their bottom edges.
beside :: [Picture] -> Picture
beside [] = []
beside ps = map trimRight (foldr1 (zipWith join) (map padded ps))
  where
    join left right = left ++ "   " ++ right
    height = maximum (map length ps)
    padded p =
      let w = maximum (0 : map length p)
          padRight s = s ++ replicate (w - length s) ' '
      in replicate (height - length p) (replicate w ' ') ++ map padRight p
    trimRight = dropWhileEnd (== ' ')

-- | A picture proof that T(n) + T(n-1) = n²: an n-by-n square, split along
-- the diagonal into T(n) 'o's and T(n-1) '.'s.
gnomonProof :: Int -> Picture
gnomonProof n = [intersperse ' ' (replicate r 'o' ++ replicate (n - r) '.') | r <- [1 .. n]]

-- | A picture proof that 2·T(n) = n(n+1): an n-by-(n+1) rectangle split
-- into two copies of T(n), one of 'o's and one of '.'s.
pronicProof :: Int -> Picture
pronicProof n = [intersperse ' ' (replicate r 'o' ++ replicate (n + 1 - r) '.') | r <- [1 .. n]]

-- | How many dots are in a picture?  (Used by the tests to check that every
-- picture really does show the number it claims to.)
dotCount :: Picture -> Integer
dotCount = fromIntegral . length . filter (/= ' ') . concat
