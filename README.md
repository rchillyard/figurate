# figurate: learning Haskell with triangular numbers

Figurate numbers count dots arranged in regular shapes. The triangular numbers
1, 3, 6, 10, 15, … are the best known, but there are many relatives:

| Family | Shape | First members |
|---|---|---|
| triangular | triangle | 1, 3, 6, 10, 15 |
| square | square | 1, 4, 9, 16, 25 |
| pentagonal, hexagonal, … | regular s-gon | 1, 5, 12, 22 / 1, 6, 15, 28 |
| pronic | n × (n+1) rectangle | 2, 6, 12, 20, 30 |
| centered hexagonal | hexagon around a dot | 1, 7, 19, 37, 61 |
| tetrahedral | stacked triangles | 1, 4, 10, 20, 35 |
| square pyramidal | stacked squares | 1, 5, 14, 30, 55 |
| pentatope | 4-D tetrahedron | 1, 5, 15, 35, 70 |

## Getting started

Install the toolchain once, using either [GHCup](https://www.haskell.org/ghcup/)
(what the Haskell community recommends) or Homebrew:

```bash
brew install ghc cabal-install
```

Then, from this directory:

```bash
cabal update
```

```bash
cabal run figurate
```

```bash
cabal run figurate -- draw centered-hexagonal 4
```

```bash
cabal test
```

```bash
cabal repl
```

The `--` separates cabal's own options from the program's arguments.

## The program

```
figurate                      overview of all the families
figurate list FAMILY [COUNT]  the first COUNT members
figurate draw FAMILY N        draw the Nth member
figurate which X              which families contain X?
figurate eureka X             X as a sum of three triangular numbers (Gauss, 1796)
figurate proof N              picture proofs that T(n)+T(n-1) = n² and 2T(n) = n(n+1)
figurate identities [N]       check a dozen identities for n = 1 .. N
```

For example, `which 36` reports that 36 is the 8th triangular number, the 6th
square number, and so on.

## Reading order

The modules are designed to be read in this order. Each one introduces new
Haskell ideas, and the comments explain them as they come up.

1. **[src/Figurate/Triangular.hs](src/Figurate/Triangular.hs)** defines
   T(n) in six different ways.
   *Type signatures, guards, recursion, `where` and `let`, partial application,
   folds, laziness and infinite lists, `scanl`, `zipWith`, a list defined in
   terms of itself, `Maybe`, function composition, point-free style.*
2. **[src/Figurate/Family.hs](src/Figurate/Family.hs)** generalises to whole families.
   *Algebraic data types, constructors with fields, pattern matching,
   `deriving`, association lists and `lookup`, searching an infinite list,
   `fmap`/`<$>`, parsing with `readMaybe`.*
3. **[src/Figurate/Picture.hs](src/Figurate/Picture.hs)** draws the shapes as ASCII art.
   *Type synonyms, list comprehensions, literal patterns, higher-order
   functions (a function that takes a shape-drawing function), `foldr1`.*
4. **[src/Figurate/Identities.hs](src/Figurate/Identities.hs)** collects famous identities.
   *Records, functions as data, lambdas, `$`, comprehensions with several
   generators as nested loops, using laziness to stop at the first result.*
5. **[src/Figurate/Peano.hs](src/Figurate/Peano.hs)** builds the natural
   numbers from zero and successor, and defines T(n) on them.
   *Recursive data types, Peano's axioms as equations, literal and as-patterns,
   folds over your own type, writing `Num` and `Enum` instances by hand,
   an infinite number made possible by laziness.*
6. **[app/Main.hs](app/Main.hs)** is the command-line program.
   *IO and `do` notation, pattern matching on lists, continuations, keeping
   effects at the edge of a pure program.*
7. **[test/Spec.hs](test/Spec.hs)** holds property-based tests with QuickCheck.
   *Stating laws that should hold for all inputs, generators, and why a
   type class (`Testable`) lets one function `property` accept so many
   different kinds of argument.*

## A first session in the REPL

`cabal repl` loads the library into GHCi. First bring all its modules into
scope, then try these, and use `:t` on anything you're not sure about:

```haskell
ghci> :m + Data.Maybe Figurate.Triangular Figurate.Family Figurate.Picture Figurate.Identities Figurate.Peano
ghci> take 10 triangulars
[0,1,3,6,10,15,21,28,36,45]
ghci> :t triangulars
triangulars :: [Integer]
ghci> :t scanl
scanl :: (b -> a -> b) -> b -> [a] -> [b]
ghci> triangulars !! 1000000             -- the millionth, computed lazily
ghci> triangular (10^30)                 -- Integers have no fixed size
ghci> triangularRoot 5050
Just 100
ghci> filter isTriangular [1..100]
ghci> map (nth (Polygonal 5)) [1..10]
ghci> take 8 (members (Simplex 5))       -- 5-dimensional triangles
ghci> indexIn Pronic 42
Just 6
ghci> traverse (mapM_ putStrLn) (picture (Centered 6) 4)
ghci> filter (\x -> isTriangular x && isJust (indexIn (Polygonal 4) x)) [1..100000]
ghci> 3 :: Nat                           -- Peano numbers
S (S (S Z))
ghci> triangularNat 4 == 10
True
ghci> succ (2 :: Nat) < infinity
True
ghci> :set +s                            -- show timings
ghci> triRecursive 1000000               -- compare with triClosedForm 1000000
ghci> :r                                 -- reload after editing a file
```

(The second-to-last `filter` finds numbers that are both triangular and square: 1, 36, 1225, 41616.)

## Exercises

Roughly in order of difficulty:

1. Add `star` numbers (6n(n−1) + 1: 1, 13, 37, 73, …) to `namedFamilies`.
   Are they a `Centered` family? Which one?
2. Use `which`, or GHCi, to find the numbers below 10⁶ that are both triangular
   and pentagonal.
3. Add the identity T(m + n) = T(m) + T(n) + mn. It has two variables, so it
   doesn't fit `Integer -> Bool`. Change `Identity`, or add a second kind.
4. `indexIn` searches linearly. Write a closed-form inverse for `Polygonal s`
   using `isqrt` (like `triangularRoot`), and add a QuickCheck property that
   it agrees with `indexIn`.
5. Draw `Centered 3` (centered triangular numbers) in `Picture.hs`. The
   "every picture has the right number of dots" property will tell you if
   you've got it right.
6. Replace the `Family` constructors' `Integer` fields with a `newtype Sides`
   that can only be built with values >= 3 (a "smart constructor").
7. In `Peano.hs`, define exponentiation `power :: Nat -> Nat -> Nat` from
   Peano-style equations, and add a property checking it against `^`.
   Then rewrite `add` and `mul` using `foldNat`.
8. Write `instance Show` for `Picture` by hand. Why can't you, while it's a
   `type` synonym, and what changes if you make it a `newtype`?
