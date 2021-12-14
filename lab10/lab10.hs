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

type Clause a = [Literal a]

maxClause :: (Ord a, Enum a) => [Clause a] -> a
maxClause = maximum . map maximum . map (map getAtom)

clauseRuleMax :: Enum a => Clause a -> a -> [Clause a]
clauseRuleMax [] _ = []
clauseRuleMax [l] x =
  let p = succ x in
    (clauseRule [l, P p] p) ++ (clauseRule [l, N p] p)
clauseRuleMax [l, m] x =
  let p = succ x in
    [[l, m, P p], [l, m, N p]]

to3CNF :: Enum a => [Clause a] -> [Clause a]
to3CNF [] = []
to3CNF (c:cs) = to3CNF cs nextMax
  where
    currentMax = maxClause (c:cs)
    nc = clauseRuleMax c currentMax
    nextMax = ?

findSub :: Formula a -> Maybe (Formula a)

findSub p@(Not (Atom _)) = p
findSub p@(Bin _ (Atom _) (Atom )) = p

findSub T = Nothing
findSub F = Nothing
findSub (Atom _) = Nothing

findSub (Bin op p q) =
  case findSub p of
    Just f -> Just f
    Nothing -> findSub p
findSub (Not p) = findSub p

toTseitinMax :: (Eq a, Enum a) => Formula a -> a -> Formula a
toTseitinMax p x =
  case findSub p of
    Just q -> Bin Conj equivalencia (toTseitinMax nuevaFormula nuevaVar)
      where
        nuevaVar = succ x
        equivalencia = Bin Equiv (Atom nuevaVar) p
        nuevaFormula = applySubst (toSubst [(q, Atom nuevaVar)]) p
    Nothing -> p
