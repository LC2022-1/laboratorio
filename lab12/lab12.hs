-- | Mark for literals
data Literal a
  = P {getAtom :: a}
  | N {getAtom :: a} deriving (Eq)

instance Show a => Show (Literal a) where
  show (P a) = show a
  show (N a) = "~" ++ show a

-- | Allowed binary operators for formulas
data Op
  = Dis -- ^ Disjunction (Or)
  | Conj -- ^ Conjunction (And)
  | Impl -- ^ Implication
  | Equiv -- ^ Logical equivalence
  deriving (Eq, Enum, Show)

-- | Propositional formulas.
data Formula a
  = T
  | F
  | Atom a
  | Not (Formula a)
  | Bin Op (Formula a) (Formula a)
  deriving (Eq, Show)

toNnf :: Formula a -> Formula a
toNnf = undefined

toCnf :: Formula a -> Formula a
toCnf p = case toNnf p of
  Bin Conj x1 x2 -> distribuye $ Bin Conj (toCnf x1) (toCnf x2)
  Bin Dis x1 x2 -> distribuye $ Bin Dis (toCnf x1) (toCnf x2)
  q -> q

toDnf :: Formula a -> Formula a
toDnf = undefined

distribuye = undefined

-- | Formula classification
data Class a
  = Alpha {left :: Formula a, right :: Formula a} -- ^ Alpha fórmula
  | Beta {left :: Formula a, right :: Formula a} -- ^ Beta formula
  | Lit {lit :: Literal a} -- ^ Logical literals
  | Na {atom :: Formula a} -- ^ For logical constants
  deriving Show

classify :: Formula a -> Class a
classify (Not (Not p)) = Alpha p T

-- | Tree structur for a semantic tableau
data Tableau a
  = Tableau
    {formulas :: [Formula a] -- ^ Set of non-literal formulas
    ,literals :: [Literal a] -- ^ Set of literals
    ,children :: [Tableau a] -- ^ Subtrees of the tableau
    }

instance Show a => Show (Tableau a) where
  show t = unlines . showLevels $ t

-- | Shows a tableau as a file-tree like structure
showLevels :: Show a => Tableau a -> [String]
showLevels (Tableau fs ls chs) =
  ("phis: " ++ show fs)
  : ("lits: " ++ show ls)
  : (tabSubTrees chs)

-- | Show subtrees using file-tree like symbols and indendation
tabSubTrees :: Show a => [Tableau a] -> [String]
tabSubTrees [] = []
tabSubTrees xs =
  (init xs >>= showPr "├── " "│   ")
  ++ (showPr "└── " "    " . last $ xs)
    where showPr f r = tabPriority f r . showLevels

-- | Add prefix to each string, with the first element getting a special
-- prefix.
tabPriority :: String -> String -> [String] -> [String]
tabPriority _ _ [] = []
tabPriority f r (x:xs) = (f ++ x) : (map (r ++) xs)

makeTableau :: Formula a -> Tableau a
makeTableau p = evolve $ Tableau [p] [] []

evolve :: Tableau a -> Tableau a
evolve t@(Tableau [] _ _) = t
evolve t@(Tableau (p:ps) lits []) =
  case classify p of
    Na T -> evolve $ Tableau ps lits []
    Na F -> Tableau (F:ps) lits []
    Lit x1 -> evolve $ Tableau ps (x1:lits) []
    Alpha x1 x2 -> undefined
    Beta x1 x2 -> undefined
