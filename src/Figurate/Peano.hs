-- | Peano's natural numbers, built from nothing but zero and successor.
--
-- Giuseppe Peano (1889) defined the naturals by two rules: 0 is a natural
-- number, and the successor S(n) of a natural number n is a natural number.
-- In Haskell that definition is a one-line data type.  Everything else
-- here (addition, multiplication, subtraction, ordering, even T(n))
-- follows from it by recursion.
--
-- This is a teaching module: each number is a chain of S's as long as its
-- value, so it's hopelessly slow for real arithmetic.  Real programs use
-- 'Natural' (from "Numeric.Natural"), which is stored in binary.
module Figurate.Peano
  ( Nat (..)
  , add
  , mul
  , minus
  , foldNat
  , toNatural
  , fromNatural
  , triangularNat
  , infinity
  ) where

import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)

-- | Three is @S (S (S Z))@.
--
-- Derived 'Ord' compares constructors in the order they're declared, so
-- Z < S n for every n, and @S m@ versus @S n@ compares m with n.  That is
-- exactly the usual order on the naturals, and we got it for free.
data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

-- | Peano's axioms for addition, written as Haskell equations:
--
-- > m + 0    = m
-- > m + S(n) = S(m + n)
add :: Nat -> Nat -> Nat
add m Z     = m
add m (S n) = S (add m n)

-- | Likewise for multiplication:
--
-- > m × 0    = 0
-- > m × S(n) = m × n + m
mul :: Nat -> Nat -> Nat
mul _ Z     = Z
mul m (S n) = add (mul m n) m

-- | Subtraction, which can fail: there is no natural number 2 - 3.
-- Peel an S off both arguments until one of them runs out.
minus :: Nat -> Nat -> Maybe Nat
minus m     Z     = Just m
minus Z     (S _) = Nothing
minus (S m) (S n) = minus m n

-- | Replace Z with z and every S with s:
--
-- > foldNat z s (S (S Z))  =  s (s z)
--
-- 'foldr' captures the pattern of recursion over lists, and 'foldNat'
-- captures the same thing for Nat.  Every function in this module could
-- be written with it; 'toNatural' is.
foldNat :: a -> (a -> a) -> Nat -> a
foldNat z _ Z     = z
foldNat z s (S n) = s (foldNat z s n)

-- | Count the S's.
toNatural :: Nat -> Natural
toNatural = foldNat 0 (+ 1)

-- | Build a chain of n S's.  A numeric literal such as 0 can be used as a
-- pattern; it matches by comparing with '=='.
fromNatural :: Natural -> Nat
fromNatural 0 = Z
fromNatural n = S (fromNatural (n - 1))

-- | T(n), Peano style:  T(0) = 0  and  T(S(n)) = S(n) + T(n).
--
-- @sn\@(S n)@ is an /as-pattern/: it takes the argument apart (naming its
-- inner number n) and also names the whole argument sn, so we can use
-- both without rebuilding @S n@.
triangularNat :: Nat -> Nat
triangularNat Z         = Z
triangularNat sn@(S n)  = add sn (triangularNat n)

-- | A number bigger than every other: S (S (S ...)) forever.
--
-- Laziness lets us define it, and even compare with it:
-- @fromNatural 1000 < infinity@ is True, because comparison looks at only
-- as many S's as it needs.  Don't print it or convert it, though:
-- that never finishes.
infinity :: Nat
infinity = S infinity

-- | Writing an instance by hand (rather than deriving it) is how a type
-- joins a type class.  Joining 'Num' lets Nat use the ordinary arithmetic
-- operators and numeric literals: @3 :: Nat@ means @S (S (S Z))@, and
-- @2 + 3 * 4 :: Nat@ works as usual.
--
-- Like 'Natural', it throws an error for results below zero.
instance Num Nat where
  (+) = add
  (*) = mul
  m - n = fromMaybe (error "Nat: subtraction below zero") (minus m n)
  abs = id
  signum Z = Z
  signum _ = S Z
  negate Z = Z
  negate _ = error "Nat: negation of a positive number"
  fromInteger n
    | n < 0     = error "Nat: negative literal"
    | otherwise = fromNatural (fromInteger n)

-- | 'Enum' is the class that provides successor and predecessor, and for
-- Nat they're just the constructor S and taking it off again.  It also
-- makes ranges such as @[Z .. 5]@ work.
instance Enum Nat where
  succ = S
  pred Z     = error "Nat: zero has no predecessor"
  pred (S n) = n
  toEnum   = fromIntegral
  fromEnum = fromIntegral . toNatural
