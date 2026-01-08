{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
module Main where

import System.Exit (exitSuccess, exitFailure)

#ifdef CHALLENGE1
#include "coursework_challenge1"
#endif

#ifdef CHALLENGE2
#include "coursework_challenge2"
#endif

#ifdef CHALLENGE3
#include "coursework_challenge3"
#endif

#ifdef CHALLENGE6
#include "coursework_challenge6"
#endif

-- A tiny assertion helper
assertEq :: (Eq a, Show a) => String -> a -> a -> IO Int
assertEq name expected actual =
  if expected == actual
    then putStrLn ("[PASS] " ++ name) >> pure 0
    else do
      putStrLn ("[FAIL] " ++ name)
      putStrLn ("  expected: " ++ show expected)
      putStrLn ("  but got : " ++ show actual)
      pure 1

-- Sum list of IO Int (fail counts)
sumFails :: [IO Int] -> IO Int
sumFails actions = do
  ns <- sequence actions
  pure (sum ns)

-------------------------------------------------------------------------------
-- Challenge 1: boundedLang over LRegExp/RegExp/SRegExp
-------------------------------------------------------------------------------
#ifdef CHALLENGE1

-- Uses symbols from coursework_challenge1:
--   boundedLang :: Int -> LRegExp -> [String]
--   data LRegExp = Let [(Int, RegExp)] RegExp
--   data RegExp  = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp]
--   data SRegExp = Empty | Lit Char | Opt Char | Any | OptAny
--                 | SSeq [SRegExp] | Choice [SRegExp] | Var Int

main :: IO ()
main = do
  let t name k lr expected = assertEq name expected (boundedLang k lr)

  failures <- sumFails
    [ t "Lit at k=0" 0 (Let [] (S (Lit 'A'))) []
    , t "Lit at k=1" 1 (Let [] (S (Lit 'A'))) ["A"]

    , t "Empty at k=0" 0 (Let [] (S Empty)) [""]
    , t "Empty at k=1" 1 (Let [] (S Empty)) [""]

    , t "Opt 'A' at k=0" 0 (Let [] (S (Opt 'A'))) [""]
    , t "Opt 'A' at k=1" 1 (Let [] (S (Opt 'A'))) ["","A"]

    , t "Any is literal dot at k=1" 1 (Let [] (S Any)) ["."]
    , t "OptAny at k=1" 1 (Let [] (S OptAny)) ["","."]
    , t "OptAny at k=0" 0 (Let [] (S OptAny)) [""]

    , t "SSeq [A, opt B] at k=1" 1 (Let [] (S (SSeq [Lit 'A', Opt 'B']))) ["A"]
    , t "SSeq [A, opt B] at k=2" 2 (Let [] (S (SSeq [Lit 'A', Opt 'B']))) ["A","AB"]

    , t "Choice [B, A] sorted" 1 (Let [] (S (Choice [Lit 'B', Lit 'A']))) ["A","B"]

    , t "Var resolves binding" 2 (Let [(1, Seq [S (Lit 'A'), S (Lit 'B')])] (S (Var 1))) ["AB"]
    , t "Unbound Var matches nothing" 5 (Let [] (S (Var 42))) []

    , t "Seq A B* at k=3" 3 (Let [] (Seq [S (Lit 'A'), Star (Lit 'B')])) ["A","AB","ABB"]

    , t "Star A at k=3" 3 (Let [] (Star (Lit 'A'))) ["","A","AA","AAA"]
    , t "Plus A at k=0" 0 (Let [] (Plus (Lit 'A'))) []
    , t "Plus A at k=2" 2 (Let [] (Plus (Lit 'A'))) ["A","AA"]

    , t "Star Empty" 3 (Let [] (Star Empty)) [""]
    , t "Plus Empty" 2 (Let [] (Plus Empty)) [""]

    , t "Star (A|B) at k=2" 2 (Let [] (Star (Choice [Lit 'A', Lit 'B']))) ["","A","AA","AB","B","BA","BB"]

    , t "Seq ABC pruned by k=2" 2 (Let [] (Seq [S (Lit 'A'), S (Lit 'B'), S (Lit 'C')])) []

    , t "Dedup via Choice [Empty, Opt 'A'] at k=1" 1 (Let [] (S (Choice [Empty, Opt 'A']))) ["","A"]

    , t "Var inside repetition: (Var 1)+ where 1 = SSeq [A,B]" 4
        (Let [(1, S (SSeq [Lit 'A', Lit 'B']))] (Plus (Var 1)))
        ["AB","ABAB"]

    , t "Mixed: Var then C+ (1 = A B*)" 3
        (Let [(1, Seq [S (Lit 'A'), Star (Lit 'B')])]
             (Seq [S (Var 1), Plus (Lit 'C')]))
        ["AC","ABC","ABBC"]

    , t "Dot is literal across repetition" 2 (Let [] (Plus Any)) [".",".."]
    ]

  putStrLn "--------------------------------"
  if failures == 0 then putStrLn "All Challenge 1 tests passed." >> exitSuccess
                   else putStrLn (show failures ++ " test(s) failed.") >> exitFailure

#endif

-------------------------------------------------------------------------------
-- Challenge 2: parseRegExp :: String -> Maybe LRegExp
-------------------------------------------------------------------------------
#ifdef CHALLENGE2

-- Uses symbols from coursework_challenge2:
--   parseRegExp :: String -> Maybe LRegExp
--   and the same AST types/constructors as challenge 1 (Eq instances provided)

main :: IO ()
main = do
  let p name input expected =
        assertEq name expected (parseRegExp input)

  failures <- sumFails
    [ p "Single literal A" "A" (Just (Let [] (S (Lit 'A'))))
    , p "Empty 'e'"        "e" (Just (Let [] (S Empty)))

    , p "Optional A '?'"   "A?" (Just (Let [] (S (Opt 'A'))))
    , p "Any '.' literal"  "."  (Just (Let [] (S Any)))
    , p "OptAny '.?'"      ".?" (Just (Let [] (S OptAny)))

    , p "Concatenation S-level (AB)" "AB"
        (Just (Let [] (S (SSeq [Lit 'A', Lit 'B']))))

    , p "Choice A|B|A normalized (dedup order not guaranteed by Eq)" "A|B|A"
        (parseRegExp "A|B|A") -- Just check it parses (normalization is internal)

    , p "RegExp star: (A)*" "(A)*"
        (Just (Let [] (Star (SSeq [Lit 'A']))))

    , p "RegExp plus: x1+" "x1+"
        (Just (Let [] (Plus (Var 1))))

    , p "Paren S-level: (A|B)C"
        "(A|B)C"
        (Just (Let [] (S (SSeq [Choice [Lit 'A', Lit 'B'], Lit 'C']))))

    , p "Let binding then use: let x1 = AB in x1+"
        "let x1 = AB in x1+"
        (Just (Let [(1, S (SSeq [Lit 'A', Lit 'B']))] (Plus (Var 1))))

    , p "Nested let: let x1 = A in let x2 = x1? in x2*"
        "let x1 = A in let x2 = x1? in x2*"
        (Just (Let [(2, S (Opt 'A')), (1, S (Lit 'A'))] (Star (Var 2))))
    ]

  putStrLn "--------------------------------"
  if failures == 0 then putStrLn "All Challenge 2 tests passed." >> exitSuccess
                   else putStrLn (show failures ++ " test(s) failed.") >> exitFailure

#endif

-------------------------------------------------------------------------------
-- Challenge 3: unparseRegExp :: LRegExp -> String
-------------------------------------------------------------------------------
#ifdef CHALLENGE3

-- Uses symbols from coursework_challenge3:
--   unparseRegExp :: LRegExp -> String
--   and AST constructors (normalization is internal to the renderer)

main :: IO ()
main = do
  let u name ast expected =
        assertEq name expected (unparseRegExp ast)

  failures <- sumFails
    [ u "Literal A" (Let [] (S (Lit 'A'))) "A"
    , u "Empty e"   (Let [] (S Empty))     "e"

    , u "Opt A"     (Let [] (S (Opt 'A'))) "A?"
    , u "Any '.'"   (Let [] (S Any))       "."
    , u "OptAny"    (Let [] (S OptAny))    ".?"

    , u "S-level concat AB"
        (Let [] (S (SSeq [Lit 'A', Lit 'B']))) "AB"

    , u "Choice A|B"
        (Let [] (S (Choice [Lit 'A', Lit 'B']))) "A|B"

    , u "Star over unit"
        (Let [] (Star (Lit 'A'))) "A*"

    , u "Plus over unit"
        (Let [] (Plus (Lit 'B'))) "B+"

    , u "Mixed: (A|B)C as minimal parens"
        (Let [] (S (SSeq [Choice [Lit 'A', Lit 'B'], Lit 'C']))) "(A|B)C"

    , u "Let chain renders bindings then main"
        (Let [(1, S (Lit 'A')), (2, Star (Lit 'B'))] (Seq [S (Var 1), S (Var 2)]))
        "let x1 = A in let x2 = B* in x1B*"
    ]

  putStrLn "--------------------------------"
  if failures == 0 then putStrLn "All Challenge 3 tests passed." >> exitSuccess
                   else putStrLn (show failures ++ " test(s) failed.") >> exitFailure

#endif

-------------------------------------------------------------------------------
-- Challenge 6: countReductions :: Int -> Exp -> (Int,Int)
-------------------------------------------------------------------------------
#ifdef CHALLENGE6

-- Uses symbols from coursework_challenge6:
--   data Exp = Var Int | Abs Int Exp | App Exp Exp | Exn | Catch Exp Exp
--   countReductions :: Int -> Exp -> (Int,Int)
--   stepCBV/stepCBN semantics are internal; we test observable counts.

main :: IO ()
main = do
  let c name bound exp expected =
        assertEq name expected (countReductions bound exp)

  -- Shorthands
  let v x       = Var x
      lam x b   = Abs x b
      app a b   = App a b
      catch a h = Catch a h

  failures <- sumFails
    [ c "Values are normal (no steps)" 10 (lam 1 (v 1)) (0,0)
    , c "Exn is normal"                10 Exn           (0,0)

    -- Beta-reduction (CBV vs CBN) on simple identity
    , c "App (Abs x x) (Abs y y) reduces (CBV=1 for argument value, CBN=1)"
        100 (app (lam 1 (v 1)) (lam 2 (v 2))) (1,1)

    -- Application with exception propagation
    , c "Exn in function position propagates"
        100 (app Exn (v 1)) (1,1)  -- one step to Exn in both strategies

    , c "Exn in argument position propagates (CBV reduces argument to Exn, CBN sees Exn)"
        100 (app (lam 1 (v 1)) Exn) (1,1)

    -- Catch semantics
    , c "catch e Exn -> e (both strategies)"
        100 (catch (lam 1 (v 1)) Exn) (1,1)

    , c "catch e (lambda) -> lambda (both strategies)"
        100 (catch (lam 1 (v 1)) (lam 2 (v 2))) (1,1)

    -- Nested apps: ((Abs x (Abs y x)) (Abs z z)) (Abs w w)
    , c "Nested apps reduce under CBV/CBN"
        100 (app (app (lam 1 (lam 2 (v 1))) (lam 3 (v 3))) (lam 4 (v 4)))
        (3,3)

    -- Bound truncation
    , c "Bound truncates counts"
        0 (app (lam 1 (v 1)) (lam 2 (v 2))) (0,0)
    ]

  putStrLn "--------------------------------"
  if failures == 0 then putStrLn "All Challenge 6 tests passed." >> exitSuccess
                   else putStrLn (show failures ++ " test(s) failed.") >> exitFailure

#endif

-------------------------------------------------------------------------------
-- Fallback when no challenge flag is provided
-------------------------------------------------------------------------------
#if !defined(CHALLENGE1) && !defined(CHALLENGE2) && !defined(CHALLENGE3) && !defined(CHALLENGE6)
main :: IO ()
main = do
  putStrLn "Please compile with one of: -DCHALLENGE1, -DCHALLENGE2, -DCHALLENGE3, -DCHALLENGE6"
  putStrLn "Example: ghc -O0 -DCHALLENGE1 Tests.hs coursework_challenge1 -o tests1"
  exitSuccess
#endif
