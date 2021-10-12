-- | Propositional logic

module Logic.Propositions where

data IFormula
  = Z
  | O
  | Var String
  | Neg IFormula
  | IFormula :&: IFormula
  | IFormula :|: IFormula
  | IFormula :->: IFormula
  | IFormula :<->: IFormula deriving (Show)

infixr 7 :&:
infixr 6 :|:
infixr 5 :->:
infixr 4 :<->:

data Formula
  = T
  | F
  | Atom String
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Impl Formula Formula
  | Equiv Formula Formula deriving (Show)

fromInfix :: IFormula -> Formula
fromInfix Z = F
fromInfix O = T
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

eval :: Inter -> Formula -> Bool
eval i T = True
eval i F = False
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
