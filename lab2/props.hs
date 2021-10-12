-- | Propositional logic

module Logic.Propositions where

data Formula
  = F
  | T
  | Var String
  | Neg Formula
  | Formula :&: Formula
  | Formula :|: Formula
  | Formula :->: Formula
  | Formula :<->: Formula deriving (Show)

infixr 7 :&:
infixr 6 :|:
infixr 5 :->:
infixr 4 :<->:

data FormulaP
  = O
  | Z
  | Atom String
  | Not FormulaP
  | And FormulaP FormulaP
  | Or FormulaP FormulaP
  | Impl FormulaP FormulaP
  | Equiv FormulaP FormulaP deriving (Show)

fromInfix :: Formula -> FormulaP
fromInfix F = Z
fromInfix T = O
fromInfix (Var s) = Atom s
fromInfix (Neg p) = Not (fromInfix p)
fromInfix (p :&: q) = And (fromInfix p) (fromInfix q)
fromInfix (p :|: q) = Or (fromInfix p) (fromInfix q)
fromInfix (p :->: q) = Impl (fromInfix p) (fromInfix q)
fromInfix (p :<->: q) = Equiv (fromInfix p) (fromInfix q)

type Inter = String -> Maybe Bool

i :: Inter
i "p" = Just True
i "q" = Just True
i "r" = Just False
i _ = Nothing

eval :: Inter -> FormulaP -> Bool
eval i O = True
eval i Z = False
eval i (Atom v) =
  let val = i v in
    case val of
      Nothing -> error "undefined variable"
      Just p -> p
eval i (Not p) = not $ eval i p
eval i (And p q) = eval i p && eval i q
eval i (Or p q) = eval i p || eval i q
eval i (Impl p q) = not (eval i p) || eval i q
eval i (Equiv p q) = eval i (Impl p q) && eval i (Impl q p)
