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
eval (K g v) w e = foldExp (\x -> elem w (v x)) (not) (||) (&&) (\_ -> mundoVecino (\x y -> x||y) False)  (\_ -> mundoVecino (\x y -> x&&y) True) e -- faltan definir las dos ultimas funciones
					where mundoVecino f casoBase = foldr (\w2 b -> f (eval (K g v) w2 e) b) casoBase (vecinos g w)

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn = undefined

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar = undefined

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto = undefined

