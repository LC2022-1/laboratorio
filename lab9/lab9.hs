-- | Mark for literals
data Literal a
  = P {getAtom :: a}
  | N {getAtom :: a} deriving Eq

instance Show a => Show (Literal a) where
  show (P a) = show a
  show (N a) = "~" ++ show a
-- | Allowed binary operators for formulas
data Op
  = Dis -- ^ Disjunction (Or)
  | Conj -- ^ Conjunction (And)
  | Impl -- ^ Implication
  | Equiv -- ^ Logical equivalence
  deriving (Eq, Enum)

-- | Propositional formulas.
-- Using polish notation
data Formula a
  = T
  | F
  | Atom a
  | Not (Formula a)
  | Bin Op (Formula a) (Formula a)
  deriving (Eq)

instance Show Op where
  show Dis = "|"
  show Conj = "&"
  show Impl = "->"
  show Equiv = "<->"

instance Show a => Show (Formula a) where
  show T = "T"
  show F = "F"
  show (Atom a) = show a
  show (Not p) = "¬" ++ show p
  show (Bin op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

