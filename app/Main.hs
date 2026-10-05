-- | The command-line program.  Everything with side effects (reading
-- arguments, printing) lives here, in the IO type.  The library modules
-- are pure: given the same inputs, their functions always return the same
-- outputs and do nothing else.
module Main (main) where

import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hSetEncoding, stdout, utf8)
import Text.Read (readMaybe)

import Figurate.Family
import Figurate.Identities
import Figurate.Picture
import Figurate.Triangular (triangularRoot)

-- | @do@ notation sequences IO actions.  @args <- getArgs@ runs an action
-- and names its result.  Then we pattern-match on the list of arguments.
main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  case args of
    []                -> overview
    ["families"]      -> listFamilies
    ["list", f]       -> withFamily f (\fam -> listMembers fam 15)
    ["list", f, k]    -> withFamily f (\fam -> withNumber k (listMembers fam))
    ["draw", f, k]    -> withFamily f (\fam -> withNumber k (draw fam))
    ["which", x]      -> withNumber x which
    ["eureka", x]     -> withNumber x eureka
    ["proof", k]      -> withNumber k proof
    ["identities"]    -> runIdentities 1000
    ["identities", k] -> withNumber k runIdentities
    _                 -> usage >> exitFailure

usage :: IO ()
usage = putStr $ unlines
  [ "Usage:"
  , "  figurate                      overview of all the families"
  , "  figurate families             list the family names"
  , "  figurate list FAMILY [COUNT]  the first COUNT members (default 15)"
  , "  figurate draw FAMILY N        draw the Nth member"
  , "  figurate which X              which families contain X?"
  , "  figurate eureka X             write X as a sum of three triangular numbers"
  , "  figurate proof N              picture proofs about T(N)"
  , "  figurate identities [N]       check the identities for n = 1 .. N"
  , ""
  , "FAMILY is a name such as triangular, or polygonal:S, centered:S,"
  , "pyramidal:S or simplex:D, e.g. polygonal:11 or simplex:5."
  ]

-- | These helpers take the "rest of the program" as a function argument
-- (a continuation), and call it only if the input made sense.
withFamily :: String -> (Family -> IO ()) -> IO ()
withFamily name continue =
  case parseFamily name of
    Just fam -> continue fam
    Nothing  -> die ("Unknown family: " ++ name ++ "  (try: figurate families)")

withNumber :: String -> (Integer -> IO ()) -> IO ()
withNumber s continue =
  case readMaybe s of
    Just x | x >= 0 -> continue x
    _               -> die ("Expected a non-negative whole number, not: " ++ s)

overview :: IO ()
overview = do
  putStrLn "The first ten members of each family:\n"
  mapM_ showFamily namedFamilies
  putStrLn ""
  usage
  where
    showFamily (name, fam) =
      putStrLn (padRight 21 name ++ concatMap (padLeft 6 . show) (take 10 (members fam)))

listFamilies :: IO ()
listFamilies =
  mapM_ (\(name, fam) -> putStrLn (padRight 21 name ++ show fam)) namedFamilies

listMembers :: Family -> Integer -> IO ()
listMembers fam count = do
  putStrLn (familyName fam ++ " numbers:")
  putStrLn (intercalate ", " (map show (take (fromInteger count) (members fam))))

draw :: Family -> Integer -> IO ()
draw fam n
  | n < 1 || n > 30 = die "Please choose 1 <= N <= 30."
  | otherwise =
      case picture fam (fromInteger n) of
        Just pic -> do
          mapM_ putStrLn pic
          putStrLn ""
          putStrLn ("The " ++ ordinal n ++ " " ++ familyName fam ++ " number is " ++ show (nth fam n))
        Nothing ->
          die ("No picture for " ++ familyName fam ++ " numbers yet. Pictures exist for: "
                 ++ intercalate ", " drawable)
  where
    drawable = [name | (name, f) <- namedFamilies, isJust (picture f 1)]

-- | @Just i <- [indexIn fam x]@ is a pattern in a comprehension: elements
-- that don't match the pattern (here, Nothing) are silently skipped.
which :: Integer -> IO ()
which x =
  case hits of
    [] -> putStrLn (show x ++ " isn't in any of the named families.")
    _  -> mapM_ describe hits
  where
    hits = [(fam, i) | (_, fam) <- namedFamilies, Just i <- [indexIn fam x]]
    describe (fam, i) =
      putStrLn (show x ++ " is the " ++ ordinal i ++ " " ++ familyName fam ++ " number")

eureka :: Integer -> IO ()
eureka x =
  case threeTriangulars x of
    Just (a, b, c) ->
      putStrLn ("ΕΥΡΗΚΑ!  " ++ show x ++ " = " ++ show a ++ " + " ++ show b ++ " + " ++ show c
                  ++ "  = " ++ intercalate " + " (map showT [a, b, c]))
    Nothing -> die "Gauss was wrong?!  (He wasn't: this would be a bug.)"
  where
    showT v = "T(" ++ maybe "?" show (triangularRoot v) ++ ")"

proof :: Integer -> IO ()
proof n
  | n < 1 || n > 30 = die "Please choose 1 <= N <= 30."
  | otherwise = do
      let k = fromInteger n
          tn = n * (n + 1) `div` 2
      putStrLn ("T(" ++ show n ++ ") + T(" ++ show (n - 1) ++ ") = " ++ show (n * n)
                  ++ "   (o = T(" ++ show n ++ "), . = T(" ++ show (n - 1) ++ "))\n")
      mapM_ putStrLn (gnomonProof k)
      putStrLn ("\n2 · T(" ++ show n ++ ") = " ++ show (2 * tn) ++ " = " ++ show n ++ " × " ++ show (n + 1)
                  ++ "   (two copies of T(" ++ show n ++ ") make a rectangle)\n")
      mapM_ putStrLn (pronicProof k)

runIdentities :: Integer -> IO ()
runIdentities limit = do
  putStrLn ("Checking each identity for n = 1 .. " ++ show limit ++ "\n")
  mapM_ report identities
  where
    report identity =
      putStrLn $ case firstFailure limit identity of
        Nothing -> "  ✓  " ++ identityName identity
        Just n  -> "  ✗  " ++ identityName identity ++ "   (fails at n = " ++ show n ++ ")"

-- | 1st, 2nd, 3rd, 4th, ..., 11th, 12th, 13th, ..., 21st, ...
ordinal :: Integer -> String
ordinal n = show n ++ suffix
  where
    suffix
      | n `mod` 100 `elem` [11, 12, 13] = "th"
      | otherwise =
          case n `mod` 10 of
            1 -> "st"
            2 -> "nd"
            3 -> "rd"
            _ -> "th"

padRight, padLeft :: Int -> String -> String
padRight w s = s ++ replicate (w - length s) ' '
padLeft w s = replicate (w - length s) ' ' ++ s
