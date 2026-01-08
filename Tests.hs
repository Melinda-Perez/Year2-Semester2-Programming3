{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
module Main where

import System.Exit (exitSuccess, exitFailure)

#ifdef CHALLENGE1
#include "coursework_challenge1.hs"
#endif

#ifdef CHALLENGE2
#include "coursework_challenge2.hs"
#endif

#ifdef CHALLENGE3
#include "coursework_challenge3.hs"
#endif

#ifdef CHALLENGE6
#include "coursework_challenge6.hs"
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

    , t "Var inside repetition: (Var 1)+ where 1 = SSeq [A,B] at k=4"
        4 (Let [(1, S (SSeq [Lit 'A', Lit 'B']))] (Plus (Var 1)))
        ["AB","ABAB"]

    , t "Mixed: Var then C+ (1 = A B*) at k=3"
        3 (Let [(1, Seq [S (Lit 'A'), Star (Lit 'B')])]
             (Seq [S (Var 1), Plus (Lit 'C')]))
        ["ABC","AC","ACC"]

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

main :: IO ()
main = do
  let p name input expected =
        assertEq name expected (parseRegExp input)

  failures <- sumFails
    [ -- Basic units
      p "Single literal A" "A" (Just (Let [] (S (Lit 'A'))))
    , p "Empty 'e'"        "e" (Just (Let [] (S Empty)))
    , p "Optional A '?'"   "A?" (Just (Let [] (S (Opt 'A'))))
    , p "Any '.' literal"  "."  (Just (Let [] (S Any)))
    , p "OptAny '.?'"      ".?" (Just (Let [] (S OptAny)))
    , p "Variable x12"     "x12" (Just (Let [] (S (Var 12))))

      -- S-level concatenation flattening
    , p "S-level concat AB" "AB"
        (Just (Let [] (S (SSeq [Lit 'A', Lit 'B']))))
    , p "Nested parens flatten: A(B(C))" "A(B(C))"
        (Just (Let [] (S (SSeq [Lit 'A', Lit 'B', Lit 'C']))))

      -- Choice flattening and dedup
    , p "Choice A|B|A dedup" "A|B|A"
        (Just (Let [] (S (Choice [Lit 'A', Lit 'B']))))
    , p "Choice of three: A|B|C" "A|B|C"
        (Just (Let [] (S (Choice [Lit 'A', Lit 'B', Lit 'C']))))
    , p "Choice singleton collapses: A|A" "A|A"
        (Just (Let [] (S (Lit 'A'))))
    , p "Choice order preserved on first occurrence: B|A|B|B" "B|A|B|B"
        (Just (Let [] (S (Choice [Lit 'B', Lit 'A']))))

      -- Remove Empty inside sequences
    , p "Remove Empty inside SSeq: AeB" "AeB"
        (Just (Let [] (S (SSeq [Lit 'A', Lit 'B']))))

      -- RegExp level: repetition over S-level units
    , p "RegExp star: (A)*" "(A)*"
        (Just (Let [] (Star (Lit 'A'))))  -- singleton SSeq collapses to Lit 'A'
    , p "RegExp plus: x1+" "x1+"
        (Just (Let [] (Plus (Var 1))))
    , p "(A|B)* allowed (star over S-level choice)" "(A|B)*"
        (Just (Let [] (Star (Choice [Lit 'A', Lit 'B']))))

      -- Paren S-level then concat
    , p "Paren S-level: (A|B)C" "(A|B)C"
        (Just (Let [] (S (SSeq [Choice [Lit 'A', Lit 'B'], Lit 'C']))))
    , p "Whitespace tolerant: (A|B)C with spaces"
        "  ( A | B )   C  "
        (Just (Let [] (S (SSeq [Choice [Lit 'A', Lit 'B'], Lit 'C']))))

      -- Let chains (right-associated and accumulated)
    , p "Let binding then use: let x1 = AB in x1+"
        "let x1 = AB in x1+"
        (Just (Let [(1, S (SSeq [Lit 'A', Lit 'B']))] (Plus (Var 1))))
   
      -- Complex normalization example from brief
    , p "Complex normalization: (AB(CD)(EF|(GH|KL|GH)))*"
        "(AB(CD)(EF|(GH|KL|GH)))*"
        (Just (Let [] (Star (SSeq
          [ Lit 'A'
          , Lit 'B'
          , Lit 'C'
          , Lit 'D'
          , Choice
              [ SSeq [Lit 'E', Lit 'F']
              , SSeq [Lit 'G', Lit 'H']
              , SSeq [Lit 'K', Lit 'L']
              ]
          ] ))))

      -- Dot with optional in sequence
    , p "Dot optional within sequence: A.?" "A.?"
        (Just (Let [] (S (SSeq [Lit 'A', OptAny]))))

      -- Invalid: star/plus inside choice (iterators cannot appear within SRegExp choice)
    , p "Invalid: A|B*" "A|B*" Nothing
    , p "Invalid: A|x1+" "A|x1+" Nothing

      -- Invalid: extra star
    , p "Invalid: A**" "A**" Nothing

      -- Invalid: empty parentheses
    , p "Invalid: ()" "()" Nothing

      -- Invalid: bad variable names
    , p "Invalid var 'x'" "x" Nothing
    , p "Invalid var 'xA'" "xA" Nothing
    ]

  putStrLn "--------------------------------"
  if failures == 0 then putStrLn "All Challenge 2 tests passed." >> exitSuccess
                   else putStrLn (show failures ++ " test(s) failed.") >> exitFailure

#endif

-------------------------------------------------------------------------------
-- Challenge 3: unparseRegExp :: LRegExp -> String
-------------------------------------------------------------------------------
#ifdef CHALLENGE3

main :: IO ()
main = do
  let u name ast expected =
        assertEq name expected (unparseRegExp ast)

  failures <- sumFails
    [ -- Basic literals
      u "Literal A" (Let [] (S (Lit 'A'))) "A"
    , u "Empty e"   (Let [] (S Empty))     "e"
    , u "Optional A" (Let [] (S (Opt 'A'))) "A?"
    , u "Any '.'"   (Let [] (S Any))       "."
    , u "OptAny"    (Let [] (S OptAny))    ".?"

      -- Simple sequences
    , u "S-level concat AB"
        (Let [] (S (SSeq [Lit 'A', Lit 'B']))) "AB"
    , u "Nested sequences flatten ABC"
        (Let [] (S (SSeq [Lit 'A', SSeq [Lit 'B', Lit 'C']]))) "ABC"
    , u "Empty in sequence ignored"
        (Let [] (S (SSeq [Lit 'A', Empty, Lit 'B']))) "AB"

      -- Choices
    , u "Choice A|B"
        (Let [] (S (Choice [Lit 'A', Lit 'B']))) "A|B"
    , u "Choice A|B|C with flattening"
        (Let [] (S (Choice [Lit 'A', Choice [Lit 'B', Lit 'C']]))) "A|B|C"
    , u "Singleton choice collapses to literal"
        (Let [] (S (Choice [Lit 'A']))) "A"
    , u "Choice with Empty"
        (Let [] (S (Choice [Empty, Lit 'A']))) "e|A"

      -- Sequences with choices and minimal parentheses
    , u "Sequence with choice inside requires parens"
        (Let [] (S (SSeq [Lit 'A', Choice [Lit 'B', Lit 'C'], Lit 'D']))) "A(B|C)D"
    , u "Choice with sequences inside requires parens"
        (Let [] (S (Choice [SSeq [Lit 'A', Lit 'B'], SSeq [Lit 'C', Lit 'D']]))) "AB|CD"

      -- Star and Plus operators
    , u "Star over literal"
        (Let [] (Star (Lit 'A'))) "A*"
    , u "Plus over literal"
        (Let [] (Plus (Lit 'B'))) "B+"
    , u "Star over sequence requires parens"
        (Let [] (Star (SSeq [Lit 'A', Lit 'B']))) "(AB)*"
    , u "Plus over sequence requires parens"
        (Let [] (Plus (SSeq [Lit 'A', Lit 'B']))) "(AB)+"
    , u "Star over choice requires parens"
        (Let [] (Star (Choice [Lit 'A', Lit 'B']))) "(A|B)*"
    , u "Plus over choice requires parens"
        (Let [] (Plus (Choice [Lit 'A', Lit 'B']))) "(A|B)+"

      -- Nested Let bindings
    , u "Simple let chain"
        (Let [(1, S (Lit 'A'))] (Plus (Var 1)))
        "let x1 = A in x1+"

      -- Variables alone
    , u "Variable alone"
        (Let [] (S (Var 42))) "x42"

      -- Complex mixed example
        , u "Mixed complex"
        (Let [] (Seq [ S (SSeq [Lit 'A', Choice [SSeq [Lit 'B', Lit 'C'], Lit 'D']])
                     , Star (Lit 'E')
                     ]))
        "A(BC|D)E*"
    ]

  putStrLn "--------------------------------"
  if failures == 0 then putStrLn "All Challenge 3 tests passed." >> exitSuccess
                   else putStrLn (show failures ++ " test(s) failed.") >> exitFailure

#endif


-------------------------------------------------------------------------------
-- Challenge 6: countReductions :: Int -> Exp -> (Int,Int)
-------------------------------------------------------------------------------
#ifdef CHALLENGE6

main :: IO ()
main = do
  let c name bound term expected =
        assertEq name expected (countReductions bound term)

  let v x       = Var x
      lam x b   = Abs x b
      app a b   = App a b
      exn       = Exn
      catch a h = Catch a h

  -- Assignment example terms
  let e1 = app (lam 1 (v 1)) (lam 1 (v 1))
      e2 = lam 0 (lam 1 (v 1))
      e3 = app (lam 2 (app exn (lam 1 (v 1)))) (lam 0 (v 0))
      worked = catch e1 (app e2 e3)

  failures <- sumFails
    [ -- Normal forms
      c "Lambda value is normal (no steps)" 10 (lam 1 (v 1)) (0,0)
    , c "Exn is normal"                10 exn           (0,0)

      -- Simple reductions
    , c "App (Abs x x) (Abs y y) reduces (CBV=1, CBN=1)"
        100 (app (lam 1 (v 1)) (lam 2 (v 2))) (1,1)
    , c "Exn in function position propagates"
        100 (app exn (v 1)) (1,1)
    , c "Exn in argument position (CBV: reduces argument, CBN: sees Exn directly)"
        100 (app (lam 1 (v 1)) exn) (1,1)

      -- Reductions for catch construct
    , c "catch e Exn -> e (both strategies)"
        100 (catch (lam 1 (v 1)) exn) (1,1)
    , c "catch e (lambda) -> lambda (both strategies)"
        100 (catch (lam 1 (v 1)) (lam 2 (v 2))) (1,1)

      -- Test: bound truncates
    , c "Bound truncates counts"
        0 (app (lam 1 (v 1)) (lam 2 (v 2))) (0,0)

      -- ===== FULL ASSIGNMENT EXAMPLE =====
    , c "Assignment Example: catch e1 (e2 e3)"
        100 worked (5,3)

      -- Bound truncation on the big example
    , c "Assignment Example with step bound"
        2 worked (2,2)

    ]

  putStrLn "--------------------------------"
  if failures == 0 then putStrLn "All Challenge 6 tests passed." >> exitSuccess
                   else putStrLn (show failures ++ " test(s) failed.") >> exitFailure

#endif
