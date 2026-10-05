-- | Triangular numbers, defined several different ways.
--
-- The nth triangular number T(n) = 1 + 2 + ... + n counts the dots in a
-- triangle with n dots along each side:
--
-- >       *           T(1) = 1
-- >      * *          T(2) = 3
-- >     * * *         T(3) = 6
-- >    * * * *        T(4) = 10
--
-- Every definition below computes the same function, but each one shows a
-- different Haskell idea.  The test suite (test/Spec.hs) checks that they
-- all agree.
module Figurate.Triangular
  ( -- * Several ways to define T(n)
    triRecursive
  , triAccumulating
  , triFold
  , triClosedForm
  , triangular
    -- * The whole (infinite) sequence
  , triangulars
  , triangularsKnot
    -- * Going backwards: is a number triangular?
  , isqrt
  , triangularRoot
  , isTriangular
  ) where

import Data.Maybe (isJust)

-- | 1. Plain recursion.
--
-- The first line is the type signature: a function from Integer
-- (arbitrary precision, like BigInteger) to Integer.  The "|" lines are
-- /guards/, which are tried top to bottom, and the first one that is True
-- wins.  'otherwise' is just another name for True.
triRecursive :: Integer -> Integer
triRecursive n
  | n <= 0    = 0
  | otherwise = n + triRecursive (n - 1)

-- | 2. Tail recursion with an accumulator.
--
-- Notice that there is no argument on the left of "=".  @go 0@ is a
-- /partially applied/ function: 'go' takes two arguments and we have
-- supplied only the first.  So @triAccumulating = go 0@ means
-- @triAccumulating n = go 0 n@.
--
-- Haskell is lazy, so @acc + k@ would normally be stored as an unevaluated
-- "thunk".  'seq' forces it to be evaluated now, so we don't build up a
-- long chain of pending additions.
triAccumulating :: Integer -> Integer
triAccumulating = go 0
  where
    go acc k
      | k <= 0    = acc
      | otherwise = let acc' = acc + k in acc' `seq` go acc' (k - 1)

-- | 3. A fold over a list.
--
-- @[1 .. n]@ is the list 1, 2, ..., n.  @foldr (+) 0@ replaces every
-- (:) in the list with (+) and the final [] with 0:
--
-- > foldr (+) 0 (1 : 2 : 3 : [])  =  1 + (2 + (3 + 0))
--
-- (The Prelude's 'sum' does the same job.)
triFold :: Integer -> Integer
triFold n = foldr (+) 0 [1 .. n]

-- | 4. Gauss's closed form, n(n+1)/2.
--
-- 'div' is integer division.  Writing a function name in `backticks`
-- turns it into an infix operator.
triClosedForm :: Integer -> Integer
triClosedForm n
  | n <= 0    = 0
  | otherwise = n * (n + 1) `div` 2

-- | The definition we use elsewhere: the fast one.
triangular :: Integer -> Integer
triangular = triClosedForm

-- | 5. The whole infinite sequence 0, 1, 3, 6, 10, ...
--
-- @[1 ..]@ is infinite, and that's fine: laziness means elements are only
-- computed when someone asks for them, e.g. with @take 10 triangulars@.
-- 'scanl' is like a fold that keeps every intermediate result:
--
-- > scanl (+) 0 [1, 2, 3, ...]  =  [0, 0+1, 0+1+2, 0+1+2+3, ...]
triangulars :: [Integer]
triangulars = scanl (+) 0 [1 ..]

-- | 6. The same sequence, defined in terms of itself ("tying the knot").
--
-- Each element is the previous element plus its position:
--
-- > triangularsKnot =  0 : zipWith (+) [0, 1, 3, 6, ...] [1, 2, 3, 4, ...]
--
-- This only works because the list is built lazily: to produce element
-- k+1 we need only elements 0 .. k, which already exist.
triangularsKnot :: [Integer]
triangularsKnot = 0 : zipWith (+) triangularsKnot [1 ..]

-- | Integer square root: the largest r with r*r <= n.
--
-- Newton's method on Integers, so it stays exact for numbers with hundreds
-- of digits, where a Double would lose precision.  'error' aborts the
-- program; we use it only for inputs that are a programming mistake.
isqrt :: Integer -> Integer
isqrt n
  | n < 0     = error "isqrt: negative argument"
  | n < 2     = n
  | otherwise = go n
  where
    go x =
      let y = (x + n `div` x) `div` 2
      in if y >= x then x else go y

-- | If x = T(n), return @Just n@; otherwise return @Nothing@.
--
-- 'Maybe' is Haskell's Optional: a value is either @Just something@ or
-- @Nothing@.  The test: x is triangular exactly when 8x + 1 is a perfect
-- square, because 8·T(n) + 1 = (2n + 1)².
triangularRoot :: Integer -> Maybe Integer
triangularRoot x
  | x < 0      = Nothing
  | r * r == d = Just ((r - 1) `div` 2)
  | otherwise  = Nothing
  where
    d = 8 * x + 1
    r = isqrt d

-- | Is x a triangular number?
--
-- The "." is function composition: @(isJust . triangularRoot) x@ means
-- @isJust (triangularRoot x)@.  Defining a function by composing others,
-- without naming its argument, is called "point-free" style.
isTriangular :: Integer -> Bool
isTriangular = isJust . triangularRoot
