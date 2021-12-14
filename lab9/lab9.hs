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

toNnf :: Formula a -> Formula a -> Formula a
toNnf (Bin op l r) =
  case op of
    Dis -> Bin op (toNnf l) (toNnf r)
    Conj -> Bin op (toNnf l) (toNnf r)
    Impl -> (Bin Dis (Not (toNnf l)) (toNnf r)
    _ -> undefined
toNnf (Not (Bin op p q)) =
  case op of
      Impl -> Not (Bin Conj (toNnf p) (Not (toNnf q))
      _ -> undefined

distribuye :: Formula a -> Formula a
distribuye (Bin Dis a (Bin Conj b c)) = undefined
distribuye (Bin Conj a b) = Bin Conj (distribuye a) (distribuye b)
distribuye a = a

toCnf :: Formula a -> Formula a
toCnf p = distribuye . toNnf $ p

data Literal a = N a | P a

type Clause a = [Literal a]

type ClausulalForm a = [Clause a]

toClausulal :: Formula a -> ClausulalForm a
toClausulal p =
  case toCnf p of
    (Bin Conj a b) -> toClausulal a ++ toClausulal b
    a -> [extractLits a]

extractLits :: Formula a -> Clause a
extractLits (Atom p) = [ P a ]
extractLits (Not (Atom a)) = [ N a ]
extractLits (Bin op p q) = extractLits p ++ extracLits q

clashes :: Clause a -> Clause a -> Maybe a

removeLit :: Clause a -> a -> Clause a

resolve :: Clause a -> Clause a -> Clause a
resolve c1 c2 =
  case clashes c1 c2 of
    Nothing -> error "this should never happen"
    Just l -> (removeLit c1 l) ++ (removeLit c2 l)

isTrivial :: Clause a -> Clause a -> Bool
isTrivial p = isJust $ clashes p p

isEmpty :: Clause a -> Bool
isEmpty [] = True
isEmpty _ = False

hasEmpty :: ClausulalForm a -> Bool
hasEmpty cs = any isEmpty cs

type Clashing a = (Clause a, Clause a)

clashList :: ClausulalForm a -> [Clashing a]
clashList cs = [ (a, b) | isJust $ clashes a b]

data ResolReg a = ResolReg (ClausulalForm a) [Clashing a]

diffList :: Eq a => [a] -> [a] -> [a]
diffList = undefined

someClash :: ResolReg a -> Maybe (Clashing a)
someClash (ResolReg cs historial) =
  case diffList (clashList cs) rs of
    [] -> Nothing
    (x:_) -> Just x

resolution :: ClausulalForm a -> Bool
resolution cs = applyRes $ ResolReg cs []

applyRes :: ResolReg a -> Bool
applyRes r
applyRes r@(ResolReg cs rs)
  | hasEmpty cs = False
  | otherwise =
    case someClash r of
      Nothing -> True
      Just (c1, c2) -> applyRes $ ResolReg nClause nHistorial
        where
          c = resolve c1 c2
          nClause = if isTrivial c then cs else c:cs
          nHistorial = (c1, c2):rs
