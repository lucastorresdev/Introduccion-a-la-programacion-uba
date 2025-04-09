-- ##################### Ejercicio 1 a) ###########################

-- Patter Matching
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

-- Guardas

f' :: Integer -> Integer
f' n | n == 1 = 8
     | n == 4 = 131
    | n == 16 = 16 

-- ############ ejercicio 1 b) #####################

-- Patter Matching
g :: Integer -> Integer
g 8 = 16
g 16=4
g 131=1


-- Guardas
g' :: Integer -> Integer
g' n | n==8 = 16
    | n==16 = 4
    | n == 131 = 1

-- ########### ejercicio 1 c) #########


h :: Integer -> Integer
h = f . g

k :: Integer -> Integer
k x = g(f x)

-- ############# Ejercicio 2 ##############

--a)

absoluto :: Integer -> Integer
absoluto n
    | n >= 0 = n
    | otherwise = n*(-1)

-- b)

maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x n
    | x >= 0 && x > n = x
    | n >= 0 && n > x = n
    | x < 0 && n < 0 && x*(-1)>n*(-1) = x*(-1)
    | x < 0 && n < 0 && n*(-1)>x*(-1) = n*(-1) 


-- c)

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z 
    | x > y && x > z = x
    | y > x && y > z = y
    | z > x && z > y = z

-- d) 

algunoEs0 :: Float -> Float -> String
algunoEs0 x y
    | x == 0 || y == 0 = "Alguno es cero"
    | otherwise = "Ninguno es cero"

-- Patter Matching

algunoEs0' :: Float -> Float -> String
algunoEs0' 0 0 = "Alguno es cero"
algunoEs0' 0 _ = "Alguno es cero"
algunoEs0' _ 0 = "Alguno es cero"
algunoEs0' _ _ = "Ninguno es cero"


-- e)

ambosSon0 :: Float -> Float -> String
ambosSon0 x y
    | x == 0 && y== 0 = "Ambos son ceros"
    | otherwise = "Alguno no es 0"

-- Patter Matching

ambosSon0' :: Float -> Float -> String
ambosSon0' 0 0 = "Ambos son cero"
ambosSon0' _ _ = "Alguno no es cero"

-- f)

mismoIntervalo :: Integer -> Integer -> String
mismoIntervalo x y 
    | x <=3 && y <=3 = "Ambos pertenecen al intervalo (-infinito, 3]"
    | x <=7 && x>3 && y>3 && y <=7 = "Ambos pertenecen al intervalo (3,7]"
    | x > 7 && y >7 = "Ambos pertenecen al intervalo (7, infinito)"
    | otherwise = "No pertenecen al mismo intervalo"

-- g)

sumaDistinto :: Integer -> Integer -> Integer -> String
sumaDistinto x y z 
    | x /= y && x /= z && y/=z = "La suma es " ++ show (x+y+z)
    | otherwise =  "Hay un numero repetido"

-- h)

esMultiplo :: Integer -> Integer -> Bool
esMultiplo x y
    | mod y x == 0 = True
    | otherwise = False

-- i)

digitoUnidades :: Integer -> String
digitoUnidades n = "la unidad del numero es: " ++ show (mod n 10)

-- j)

digitoDecenas :: Integer -> String
digitoDecenas n = "La decena del numero es: " ++ show (mod n 100)

-- ########## EJERCICIO 3 ###########

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b
    | mod a b == 0 = True
    | otherwise = False

-- ######### EJERCICIO 4 ##########

--  a)

prodInt :: (Integer, Integer) -> (Integer, Integer) -> Integer
prodInt (x1,y1) (x2, y2) = (x1*x2)+(y1*y2)

-- b)

todoMenor :: (Integer, Integer) -> (Integer, Integer) -> Bool
todoMenor (x,y) (x1,y1)
    |x<x1 && y<y1 = True
    |otherwise = False

-- c)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x,y) (x1,y1) = sqrt((x1-x) ^ 2 + (y1-y) ^ 2)

-- d)

sumaTerna :: (Float, Float, Float) -> Float
sumaTerna (x,y,z) = x+y+z

-- e)

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (x,y,z) n
    | esMultiplo x n && esMultiplo y n && esMultiplo z n = x + y + z
    | esMultiplo x n && esMultiplo y n = x + y
    | esMultiplo x n && esMultiplo z n = x + z
    | esMultiplo y n && esMultiplo z n = y + z
    | esMultiplo x n = x
    | esMultiplo y n = y
    | esMultiplo z n = z
    | otherwise = 0


-- f) 
posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x,y,z)
    | mod x 2 == 0 = x
    | mod y 2 == 0 = y
    | mod z 2 == 0 = z
    | otherwise = 4

-- g)

crearPar :: a->b->(a,b)
crearPar a b = (a,b)

-- h)
invertir :: (a,b)->(b,a)
invertir (a,b) = (b,a)

-- ####### EJERCICIO 5 #######

funF :: Integer -> Integer
funF n
    | n <= 7 = (-n) ^ 2
    | n > 7 = - 2 * n - 1

funG :: Integer -> Integer
funG n
    | mod n 2 == 0 = div n 2
    | otherwise = 3*n+1

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (x, y, z)
    | funF x > funG x && funF y > funG y && funF z > funG z = True
    | otherwise = False

-- ##### Ejercicio 6 #####

biciesto :: Integer -> Bool
biciesto n
    | mod n 4 /= 0 || mod n 100 == 0 && mod n 400 /= 0 = False
    | otherwise = True

-- ##### Ejercicio 7 #####

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x,y,z) (x1,y1,z1)
    | (x-x1)+(y-y1)+(z-z1) < 0 = ((x-x1)+(y-y1)+(z-z1))*(-1)
    | otherwise = (x-x1)+(y-y1)+(z-z1)


-- ##### Ejercicio 8 #####

sumaUltimosDosDigitos :: Integer -> Integer 
sumaUltimosDosDigitos n = mod n 10 + mod (div n 10) 10

comparar :: Integer -> Integer -> Integer
comparar a b
    | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
    | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
    | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0