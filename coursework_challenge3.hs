-- Unparse Simple Regular Expressions with minimal parentheses and normalization.

import Data.List (nub, intercalate)

-- Data types
data LRegExp =  Let [(Int,RegExp)] RegExp 
  deriving (Eq,Show,Read)

data RegExp = S SRegExp | Star SRegExp  | Plus SRegExp | Seq [RegExp] 
  deriving (Eq,Show,Read)

data SRegExp =  Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int 
  deriving (Eq,Show,Read)

-- Public API
unparseRegExp :: LRegExp -> String
unparseRegExp (Let [] re)          = renderRegTop (normR re)
unparseRegExp (Let bindings re)    =
  let revBindings = reverse bindings
      go []     acc = acc
      go ((n,r):bs) acc =
        let rhs = renderRegTop (normR r)
        in go bs ("let x" ++ show n ++ " = " ++ rhs ++ " in " ++ acc)
  in go revBindings (renderRegTop (normR re))

-- =========================
-- Normalization (flattening and cleanup)
-- =========================

-- Normalize a RegExp recursively
normR :: RegExp -> RegExp
normR (S s)       = S (normS s)
normR (Star s)    = Star (normS s)
normR (Plus s)    = Plus (normS s)
normR (Seq rs)    = normalizeRSeq rs

-- Normalize a list of RegExp primaries into a single RegExp:
-- - Flatten nested Seq
-- - Merge adjacent S ... into one S (SSeq [...])
-- - Remove neutral S Empty elements
-- - Collapse singleton sequences
normalizeRSeq :: [RegExp] -> RegExp
normalizeRSeq xs = finalize (mergeAdjS (flattenSeq (map normR xs)))
  where
    flattenSeq :: [RegExp] -> [RegExp]
    flattenSeq = concatMap (\r -> case r of
                                   Seq rs -> flattenSeq rs
                                   other  -> [other])
    -- merge adjacent S blocks and drop S Empty
    mergeAdjS :: [RegExp] -> [RegExp]
    mergeAdjS = foldr step []
      where
        step (S s) acc
          | isEmptyS s = acc
          | otherwise  = case acc of
              (S s2 : rest) -> S (normalizeSSeq [s, s2]) : rest
              _             -> S (normS s) : acc
        step r acc = r : acc

    finalize :: [RegExp] -> RegExp
    finalize []  = S Empty
    finalize [r] = r
    finalize rs  = Seq rs

-- Normalize SRegExp:
-- - Flatten SSeq and Choice
-- - Remove Empty inside SSeq
-- - Deduplicate Choice alternatives
-- - Collapse singleton containers
normS :: SRegExp -> SRegExp
normS Empty         = Empty
normS (Lit c)       = Lit c
normS (Opt c)       = Opt c
normS Any           = Any
normS OptAny        = OptAny
normS (Var n)       = Var n
normS (SSeq xs)     = normalizeSSeq (map normS xs)
normS (Choice xs)   = normalizeSChoice (map normS xs)

-- Flatten SSeq, drop Empty, collapse nested sequences, simplify singleton/empty
normalizeSSeq :: [SRegExp] -> SRegExp
normalizeSSeq xs =
  let flat = concatMap (\s -> case s of
                                SSeq ys -> ys
                                other   -> [other]) xs
      noEmpty = filter (not . isEmptyS) flat
  in case noEmpty of
       []  -> Empty
       [y] -> y
       ys  -> SSeq ys

-- Flatten Choice, remove duplicates, and simplify singleton/empty
normalizeSChoice :: [SRegExp] -> SRegExp
normalizeSChoice xs =
  let flat = concatMap (\s -> case s of
                                Choice ys -> ys
                                other     -> [other]) xs
      ded  = nub flat
  in case ded of
       []  -> Empty
       [y] -> y
       ys  -> Choice ys

isEmptyS :: SRegExp -> Bool
isEmptyS Empty = True
isEmptyS _     = False

-- =========================
-- Rendering with minimal parentheses
-- =========================

-- Top-level RegExp rendering:
-- - If it's a sequence of more than one item, render items in unit-context to avoid precedence issues.
-- - If single item, render it in top-context (minimal parentheses).
renderRegTop :: RegExp -> String
renderRegTop (Seq rs) =
  case rs of
    []    -> renderSRoot Empty
    [r]   -> renderRegTop r
    _     -> concatMap renderRegItem rs
renderRegTop (S s)    = renderSRoot s
renderRegTop (Star s) = renderUnit s ++ "*"
renderRegTop (Plus s) = renderUnit s ++ "+"

-- Render a RegExp item inside a concatenation context (must be unit-safe).
renderRegItem :: RegExp -> String
renderRegItem (S s) =
  case s of
    SSeq xs | length xs > 1 -> concatMap renderUnit xs
    _                       -> renderUnit s
renderRegItem (Star s) = renderUnit s ++ "*"
renderRegItem (Plus s) = renderUnit s ++ "+"
renderRegItem (Seq rs) =
  case rs of
    []  -> renderUnit Empty
    [r] -> renderRegItem r
    _   -> concatMap renderRegItem rs

-- Root SRegExp rendering (minimal parentheses):
-- - Sequences print as concatenation of unit items.
-- - Choices print with '|' and render each alternative as a sequence.
renderSRoot :: SRegExp -> String
renderSRoot Empty        = "e"
renderSRoot (Lit c)      = [c]
renderSRoot (Opt c)      = [c] ++ "?"
renderSRoot Any          = "."
renderSRoot OptAny       = ".?"
renderSRoot (Var n)      = "x" ++ show n
renderSRoot (SSeq xs)    = concatMap renderUnit xs
renderSRoot (Choice xs)  = intercalate "|" (map renderAlt xs)

-- Render an alternative within a Choice.
-- Each alternative is rendered as a sequence of unit items (or 'e').
renderAlt :: SRegExp -> String
renderAlt Empty      = "e"
renderAlt (SSeq xs)  = concatMap renderUnit xs  -- never parenthesize here
renderAlt s          = renderUnit s

-- Render a SRegExp as a UnitRegExp string.
-- Parenthesize when the expression isn't a unit (i.e., multi-item sequence or a choice).
renderUnit :: SRegExp -> String
renderUnit Empty        = "e"
renderUnit (Lit c)      = [c]
renderUnit (Opt c)      = [c] ++ "?"
renderUnit Any          = "."
renderUnit OptAny       = ".?"
renderUnit (Var n)      = "x" ++ show n
renderUnit (SSeq xs) =
  case xs of
    []   -> "e"
    [y]  -> renderUnit y
    _    -> "(" ++ concatMap renderUnit xs ++ ")" -- single set of parens only here
renderUnit (Choice xs)  = "(" ++ renderSRoot (Choice xs) ++ ")"
