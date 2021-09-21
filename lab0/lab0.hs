-- Dos guiones para comentar
{-
Llave guión para varias líneas
-}

-- hay enteros
n :: Int
n = 5

-- números de punto flotante
x :: Float
x = 5.6

-- caractéres
c :: Char
c = 'c'

-- se puede usar haskell como una calculadora
suma = 1 + 3
resta = 2 - 10
producto = 4 * 5
division = 64 / 8
modulo = 18 `mod` 13
sucesor = succ 4
predecesor = pred 10
potencia = 3 ^ 2

-- una calculadora con valores booleanos también
conjuncion = True && False
disyuncion = False || True
negacion = not False

-- como lista de longitud fija
tpl :: (Int, Int)
tpl = (1, 2)

primero = fst tpl -- primer elemento
segundo = snd tpl -- segundo elemento

-- declara funciones es muy fácil
masUno x = x + 1

-- es buena práctica ponerle el tipo a las funciones
sumaDos :: Int -> Int
sumaDos x = x + 2

-- los símbolos válidos son !#$%&*+./<=>?@\^|-~:
(#) :: Int -> Int -> Int
n # m = m + n * m

-- esta función se aplica un argumento a la vez
sumaMasTres :: Int -> Int -> Int
sumaMasTres x y = x + y + 3

-- sumaMasTres 1 2 crea
sumaMasTres' :: Int -> Int
sumaMasTres' y = 1 + y + 3
-- y finalmente evalua 1 + 2 + 3 = 6

-- se puede tener una aplicación parcial explícita
mod5 :: Int -> Int
mod5 = (`mod` 5)

-- repetitivo
mulTupla :: Int -> (Int, Int) -> (Int, Int)
mulTupla n (a, b) = (sumaDos n * a, sumaDos n * b)

-- más compacto
mulTupla' :: Int -> (Int, Int) -> (Int, Int)
mulTupla' n (a, b) =
  let m = sumaDos n in (m * a, m * b)

-- una alternativa
mulTupla'' :: Int -> (Int, Int) -> (Int, Int)
mulTupla'' n (a, b) =
  (m * a, m * b) where m = sumaDos n

-- crear funciones desechables
cubosTupla :: (Int, Int) -> (Int, Int)
cubosTupla (a, b) =
  (cubo a, cubo b)
  where cubo = \x -> x^3

-- amabas ramas mismo tipo
absoluto :: Float -> Float
absoluto x =
  if x < 0
     then -x
     else x

-- para evitar ifs anidados
bordeAlfabeto :: Char -> Bool
bordeAlfabeto c =
  case c of
    'a' -> True
    'z' -> True
    _ -> False

-- otra sintaxis para casos
bordeAlfabeto' :: Char -> Bool
bordeAlfabeto' 'a' = True
bordeAlfabeto' 'z' = True
bordeAlfabeto' _ = False

-- función recursiva sencilla
par :: Int -> Bool
par 0 = True
par 1 = False
par n = par (n - 2)

-- fibonacci
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- otra manera de eveitar ifs anidados
grado :: Int -> String
grado g
  | g < 6 = "NA"
  | g <= 12 = "Primaria"
  | g <= 15 = "Secundaria"
  | g <= 18 = "Preparatoria"
  | otherwise = "Terminaste"

-- funciones normales
f1 x = x + 1
f2 x = 2 * x

-- función hecha de la composición de otras funciones
h = f1 . f2

-- función hecha de la aplicación de otras funciones
k x = h . f1 . f2 $ x

-- usando corchetes, como en python
lista = [1, 5, 8, 0]

multsDeCinco :: [Int] -> [Int]
-- se define para la lista vacía
multsDeCinco [] = []
-- y para elemento seguido de lista
multsDeCinco (x:xs) =
  if x `mod` 5 == 0
     then x:resto
     else resto
  where resto = multsDeCinco xs

-- algunas operaciones con listas
concatenada = [1, 2, 3] ++ [4, 5, 6]
cabeza = head [1, 2, 3]
rabo = tail [4, 5, 6]
longitud = length [8, 7, 6]

-- rango
diez = [1..10]

-- rango con paso
paresVeinte = [0, 2..20]

-- no muere porque haskell es perezoso
quince = take 15 [1..]

-- lista de los números menores a n al cuadrado
cuadrados :: Int -> [Int]
cuadrados n = [m^2 | m <- [1..n]]

-- factore de un número usando listas por comprensión
facts :: Int -> [Int]
facts n = [m | m <- [1..n], n `mod` m == 0]

-- otro ejemplo de listas por comprensión
quickSort [] = []
quickSort (x:xs) =
  let menores = [y | y <- xs, y <= x]
      mayores = [y | y <- xs, y > x]
  in quickSort menores ++ [x] ++ quickSort mayores

-- solo mayúsculas usando un filtro
mayusculas :: String -> String
mayusculas = filter (\x -> x `elem` ['A'..'Z'])

-- eliminar repeticiones usando un filtro
unicos :: Eq a => [a] -> [a]
unicos [] = []
unicos (x:xs) = x:unicos (eliminaX xs)
  where eliminaX = filter (/=x)

-- otro ejemplo de filtro
mulsFiltro :: [Int] -> [Int]
mulsFiltro = filter ((==0) . (`mod` 5))

-- sacar el inverso de los números de una lista
inversoRec :: [Int] -> [Int]
inversoRec [] = []
inversoRec (x:xs) = (-x):xs

-- misma función pero usando un mapeo
inversoMap :: Num a => [a] -> [a]
inversoMap = map negate

-- suma recursiva
sumaRec :: Num a => [a] -> a
sumaRec [] = 0
sumaRec (x:xs) = x + sumaRec xs

-- suma usando reducción (fold)
sumaFold :: Num a => [a] -> a
sumaFold = foldr (+) 0

-- otro ejemplo de fold
mayor10 :: [Int] -> Bool
mayor10 = foldr (\x acc -> acc && x > 10) True

-- explota
dividir n 0 = error "no se puede"
dividir n m = n `div` m

-- otro nombre para String
type Nombre = String

-- expresiones de sumas
data Expr = Val Int |
            Var Nombre |
            Suma Expr Expr deriving Eq

-- función para representar como cadena
instance Show Expr where
  show (Val val) = show val
  show (Var nombre) = nombre
  show (Suma e1 e2) = (show e1) ++ "+" ++ (show e2)

-- Como se hacían con listas
subst :: Expr -> Nombre -> Int -> Expr
subst (Val v) _ _ = Val v
subst (Var n) m v =
  if n == m
     then (Var m)
     else Var n
subst (Suma e1 e2) m v =
  Suma (subst e1 m v) (subst e2 m v)

-- evaluar expresiones
eval :: Expr -> Int
eval (Val val) = val
eval (Var nombre) = 0
eval (Suma e1 e2) = (eval e1) + (eval e2)

-- un tipo con varios argumentos
data Color = RGB Int Int Int

-- repetitivo
getR :: Color -> Int
getR (RGB r _ _) = r

-- compacto
data RGBA = RGBA {
  r :: Int,
  g :: Int,
  b :: Int,
  a :: Int
}

-- nunca habrá NullPointerException
mensajeSeguro :: Maybe String -> String
mensajeSeguro (Just s) = "El mensaje es:" ++ s
mensajeSeguro Nothing = "Se perdió"
