module Lomoba where
import Grafo
import Tipos
import Parser


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp :: (Prop -> a) -> (a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (a -> a) -> Exp -> a
foldExp f1 f2 f3 f4 f5 f6 (Var p) = f1 p
foldExp f1 f2 f3 f4 f5 f6 (Not e) = f2 (foldExp f1 f2 f3 f4 f5 f6 e)
foldExp f1 f2 f3 f4 f5 f6 (Or e1 e2) = f3 (foldExp f1 f2 f3 f4 f5 f6 e1) (foldExp f1 f2 f3 f4 f5 f6 e2)
foldExp f1 f2 f3 f4 f5 f6 (And e1 e2) = f4 (foldExp f1 f2 f3 f4 f5 f6 e1) (foldExp f1 f2 f3 f4 f5 f6 e2)
foldExp f1 f2 f3 f4 f5 f6 (D e) = f5 (foldExp f1 f2 f3 f4 f5 f6 e)
foldExp f1 f2 f3 f4 f5 f6 (B e) = f6 (foldExp f1 f2 f3 f4 f5 f6 e)

-- Ejercicio 11
visibilidad :: Exp -> Integer
visibilidad e = foldExp (const 0) (const 0) max max (1+) (1+) e

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer e = foldExp lista id (++) (++) id id e
				where lista x = [x]

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval (K g v) w e = foldExp (\x -> elem w (v x)) (not) (||) (&&) (\_ -> mundoVecino (\x y -> x||y) False)  (\_ -> mundoVecino (\x y -> x&&y) True) e
					where mundoVecino f casoBase = foldr (\w2 b -> f (eval (K g v) w2 e) b) casoBase (vecinos g w)

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn e (K g v) = foldr (\n b -> if (eval (K g v) n e) then n:b else b) [] (nodos g) 

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar e (K g v) = foldr (\n (K g2 v2) -> chequear n (K g2 v2)) (K g v) (nodos g)
					where chequear n (K g2 v2) = if (eval (K g2 v2) n e) then (K g2 v2) else (K (sacarNodo n g2) v2)

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto (K g v) e = cantidad (nodos g) == cantidad (valeEn e (K g v))
					where cantidad = foldr (+) 0

