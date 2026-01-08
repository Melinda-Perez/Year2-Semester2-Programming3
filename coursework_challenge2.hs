-- Parser for simplified regular expressions with normalization.
-- Accepts chains of top-level lets (right-associated), flattens sequences/choices,
-- removes Empty in sequences, deduplicates choices, and collapses singletons.
-- Ensures '?' applies only to single uppercase literals (not variables).

import Data.Char (isDigit, isUpper, isSpace)
import Data.List (nub)
import Control.Applicative (Alternative(..), many, some)

-- Data types
data LRegExp =  Let [(Int,RegExp)] RegExp 
  deriving (Eq,Show,Read)

data RegExp = S SRegExp | Star SRegExp  | Plus SRegExp | Seq [RegExp] 
  deriving (Eq,Show,Read)

data SRegExp =  Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int 
  deriving (Eq,Show,Read)

-- Public API
parseRegExp :: String -> Maybe LRegExp
parseRegExp s = case runParser (spaces *> pLRegExp <* spaces <* eof) s of
  Just (ast, _) -> Just ast
  Nothing       -> Nothing

-- =========================
-- Minimal monadic parser
-- =========================

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp -> do
    (a, rest) <- p inp
    pure (f a, rest)

instance Applicative Parser where
  pure a = Parser $ \inp -> Just (a, inp)
  (Parser pf) <*> (Parser pa) = Parser $ \inp -> do
    (f, rest1) <- pf inp
    (a, rest2) <- pa rest1
    pure (f a, rest2)

instance Monad Parser where
  (Parser pa) >>= f = Parser $ \inp -> do
    (a, rest1) <- pa inp
    runParser (f a) rest1

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \inp ->
    case p1 inp of
      Nothing -> p2 inp
      justRes -> justRes

item :: Parser Char
item = Parser $ \inp -> case inp of
  []     -> Nothing
  (c:cs) -> Just (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- item
  if f c then pure c else empty

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

eof :: Parser ()
eof = Parser $ \inp -> if null inp then Just ((), "") else Nothing

spaces :: Parser ()
spaces = () <$ many (satisfy isSpace)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

symbol :: String -> Parser String
symbol = lexeme . string

optionalP :: Parser a -> Parser (Maybe a)
optionalP p = (Just <$> p) <|> pure Nothing

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- =========================
-- Grammar parsers
-- =========================

-- <LRegExp> ::= "let" Var '=' <RegExp> "in" <LRegExp> | <RegExp>
-- Parse zero-or-more lets, then the final RegExp. Reverse bindings so inner-first.
pLRegExp :: Parser LRegExp
pLRegExp = do
  bs <- many pLetOne
  re <- pRegExp
  pure (Let (reverse bs) re)
  where
    pLetOne :: Parser (Int, RegExp)
    pLetOne = do
      _  <- symbol "let"
      n  <- pVarNum
      _  <- symbol "="
      re <- pRegExp
      _  <- symbol "in"
      pure (n, re)

-- <RegExp> ::= <UnitRegEx>'*' | <UnitRegEx>'+' | <RegEx><RegEx> | '('<RegEx>')' | <SRegEx>
-- Parse a concatenation of primaries; normalization will minimize Seq usage.
pRegExp :: Parser RegExp
pRegExp = normalizeRSeq <$> some pRegPrimary

pRegPrimary :: Parser RegExp
pRegPrimary =
      tryStar
  <|> tryPlus
  <|> parenReg
  <|> (S <$> pSRegExp)
  where
    tryStar = do
      u <- pUnitSRegExp
      _ <- symbol "*"
      pure (Star u)
    tryPlus = do
      u <- pUnitSRegExp
      _ <- symbol "+"
      pure (Plus u)
    parenReg = do
      _ <- symbol "("
      r <- pRegExp
      _ <- symbol ")"
      pure r

-- <SRegExp> ::= <SRegEx><SRegEx> | <SRegEx>'|'<SRegEx> | <UnitRegEx>
-- Precedence: concatenation binds tighter than '|'
pSRegExp :: Parser SRegExp
pSRegExp = normalizeSChoice <$> sepBy1 pSConcat (symbol "|")

-- Concatenation: one or more UnitRegExp
pSConcat :: Parser SRegExp
pSConcat = normalizeSSeq <$> some pUnitSRegExp

-- <UnitRegExp> ::= <Var> | <C> | <C>'?' | '.' | 'e' | '(' <SRegEx> ')'
pUnitSRegExp :: Parser SRegExp
pUnitSRegExp =
      pVar
  <|> pLitOrOpt
  <|> pAnyOrOptAny
  <|> pEmpty
  <|> parenS
  where
    pVar = Var <$> pVarNum
    pLitOrOpt = do
      c <- lexeme (satisfy isUpper)
      mq <- optionalP (char '?')
      pure $ case mq of
        Just _ -> Opt c          -- '?' applies only to a single uppercase literal
        Nothing -> Lit c
    pAnyOrOptAny = do
      _ <- lexeme (char '.')
      mq <- optionalP (char '?')
      pure $ case mq of
        Just _ -> OptAny
        Nothing -> Any
    pEmpty = Empty <$ symbol "e"
    parenS = do
      _ <- symbol "("
      s <- pSRegExp
      _ <- symbol ")"
      pure s

-- Var ::= 'x' <Digits>
pVarNum :: Parser Int
pVarNum = do
  _ <- lexeme (char 'x')
  ds <- some (lexeme (satisfy isDigit))
  pure (read ds)

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

Perfect — this is a clean and complete parser for your simplified regular expressions, and it already incorporates normalization and correct handling of ?, sequences, choices, and nested lets.

The only adjustments I’d suggest to make it fully aligned with your failing test cases from tests2 are:

1️⃣ Disallow * and + over choices or variables

Currently, pRegPrimary allows * or + on any SRegExp, including Choice or Var, which causes A|B* or A|x1+ to incorrectly succeed.

We can fix it by restricting */+ to units only that are not choices:

pRegPrimary :: Parser RegExp
pRegPrimary =
