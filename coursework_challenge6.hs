-- Lambda calculus with exceptions and exception handling.
-- Provides: countReductions :: Int -> Exp -> (Int,Int)

data Exp 
  = Var Int 
  | Abs Int Exp 
  | App Exp Exp 
  | Exn 
  | Catch Exp Exp
  deriving (Eq, Show, Read)

-- Public API: number of small-step reductions until normal form (bounded)
countReductions :: Int -> Exp -> (Int,Int)
countReductions bound e =
  ( min bound (steps stepCBV e)
  , min bound (steps stepCBN e)
  )

-- Reduces until no rule applies (stuck terms are considered normal forms!)
steps :: (Exp -> Maybe Exp) -> Exp -> Int
steps step0 = go 0
  where
    go n e = case step0 e of
      Nothing -> n
      Just e' -> go (n+1) e'

-- =========================
-- Call-By-Value small-step
-- =========================
stepCBV :: Exp -> Maybe Exp
stepCBV (Abs _ _) = Nothing
stepCBV Exn       = Nothing
stepCBV (Var _)   = Nothing
stepCBV (Catch e1 e2) =
  case stepCBV e1 of
    Just e1' -> Just (Catch e1' e2)
    Nothing  -> 
      case stepCBV e2 of
        Just e2' -> Just (Catch e1 e2')
        Nothing  ->
          case e2 of
            Exn      -> Just e1
            Abs _ _  -> Just e2
            _        -> Nothing
-- propagate exn IMMEDIATELY
stepCBV (App e1 e2)
  | isExn e1  = Just Exn
  | isExn e2  = Just Exn
stepCBV (App e1 e2) =
  case stepCBV e1 of
    Just e1' -> Just (App e1' e2)
    Nothing  ->
      case e1 of
        Abs x b   ->
          case stepCBV e2 of
            Just e2' -> Just (App e1 e2')
            Nothing  ->
              case e2 of
                Exn      -> Just Exn
                _        -> Just (subst x e2 b)
        _         ->
          case stepCBV e2 of
            Just e2' -> Just (App e1 e2')
            Nothing  -> Nothing

-- =========================
-- Call-By-Name small-step
-- =========================
stepCBN :: Exp -> Maybe Exp
stepCBN (Abs _ _) = Nothing
stepCBN Exn       = Nothing
stepCBN (Var _)   = Nothing
stepCBN (Catch e1 e2) =
  case stepCBN e1 of
    Just e1' -> Just (Catch e1' e2)
    Nothing  ->
      case e2 of
        Exn      -> Just e1
        Abs _ _  -> Just e2
        _        -> case stepCBN e2 of
                      Just e2' -> Just (Catch e1 e2')
                      Nothing  -> Nothing
stepCBN (App e1 e2)
  | isExn e1  = Just Exn
  | isExn e2  = Just Exn
stepCBN (App e1 e2)
  | isAbs e1 = case e1 of
      Abs x b -> Just (subst x e2 b)
      _       -> error "Impossible: non-Abs matched in stepCBN"
  | otherwise =
      case stepCBN e1 of
        Just e1' -> Just (App e1' e2)
        Nothing  ->
          case stepCBN e2 of
            Just e2' -> Just (App e1 e2')
            Nothing  -> Nothing

-- =========================
-- Helpers
-- =========================
isAbs :: Exp -> Bool
isAbs (Abs _ _) = True
isAbs _         = False

isExn :: Exp -> Bool
isExn Exn = True
isExn _   = False

-- Substitution: [v / x]e (no reduction under lambda; avoid substituting under binding of x)
subst :: Int -> Exp -> Exp -> Exp
subst x v (Var y)
  | x == y    = v
  | otherwise = Var y
subst x v (Abs y b)
  | x == y    = Abs y b                      -- stop under binder of the same variable
  | otherwise = Abs y (subst x v b)
subst x v (App a b)     = App   (subst x v a) (subst x v b)
subst _ _ Exn           = Exn
subst x v (Catch a b)   = Catch (subst x v a) (subst x v b)
