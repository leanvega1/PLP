module Lomoba where

import qualified Data.List as List

import Grafo
import Tipos
import Parser


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
-- Fold para la estructura de expresiones de logica modal. El fold utiliza una funcion por cada
-- generador del tipo Exp.
foldExp :: (Prop -> a) -> (a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (a -> a) -> Exp -> a
foldExp fVar fNot fOr fAnd fD fB exp =
	case exp of
		(Var p) -> fVar p
		(Not e) ->  fNot (foldExp fVar fNot fOr fAnd fD fB e)
		(Or e1 e2) -> fOr (foldExp fVar fNot fOr fAnd fD fB e1) (foldExp fVar fNot fOr fAnd fD fB e2)
		(And e1 e2) -> fAnd (foldExp fVar fNot fOr fAnd fD fB e1) (foldExp fVar fNot fOr fAnd fD fB e2)
		(D e) -> fD (foldExp fVar fNot fOr fAnd fD fB e)
		(B e) -> fB (foldExp fVar fNot fOr fAnd fD fB e)

		
-- Ejercicio 11
-- Funcion que devuelve en nivel de visibilidad de una expresion de logica modal.
-- Suma uno por cada cuantificador y se queda con el maximo en oepraciones and y or.
visibilidad :: Exp -> Integer
visibilidad e = foldExp fVar fNot fOr fAnd fD fB e
    where fVar = const 0
          fNot = const 0 
          fOr = max
          fAnd = max
          fD = (1+)
          fB = (1+)

		  
-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer e = List.nub (foldExp lista id (++) (++) id id e)
	where lista x = [x]

				
-- Ejercicio 13
-- Funcion que dado un modelo y un mundo permite evaluar una expresion. Cada
-- funcion del fold recibe un parametro extra para especificar en que mundo 
-- debe evaluarse la expresion. Notemos que los operadores de la logica modal
-- cambian el mundo donde debe evaluarse la subexpresion.
eval :: Modelo -> Mundo -> Exp -> Bool
eval (K g v) w exp = (foldExp fVar fNot fOr fAnd fD fB exp) w
    where fVar = (\p w -> elem w (v p) )
          fNot = (\expRec w -> not $ expRec w ) 
          fOr = (\expRec1 expRec2 w -> expRec1 w || expRec2 w )
          fAnd = (\expRec1 expRec2 w -> expRec1 w && expRec2 w )
          fD = (\expRec w -> foldr (\w' recur -> (expRec w') || recur) False (vecinos g w) )
          fB = (\expRec w -> foldr (\w' recur -> (expRec w') && recur) True (vecinos g w) )		  


-- Ejercicio 14
-- Funcion que genera una lista de los mundos donde evaluo correctamente la expresion
-- dado un determinado modelo.
valeEn :: Exp -> Modelo -> [Mundo]
valeEn exp (K g v) = foldr f [] (nodos g)
	where f = (\w ws -> if (eval (K g v) w exp) then 
							w:ws 
						else 
							ws	
				)

				
-- Ejercicio 15
-- Funcion que dada una expresion y un modelo, genera un modelo removiendo todos los
-- mundos donde no es cierta la expresion (esto es analogo a decir donde es cierta su negacion).
-- Los mundos son removidos tanto del listado del Modelo como de la relacion.
quitar :: Exp -> Modelo -> Modelo
quitar exp (K g v) = foldr f (K g v) (valeEn (Not exp) (K g v) )
    where f = (\w (K recG recV) -> K (sacarNodo w recG) (h w recV) )
          h = (\w recV p -> if elem p (extraer exp) then 
								List.delete w (recV p)
							else
								recV p
				)


-- Ejercicio 16
-- Funcion que dado un modelo y una expresion devuelve verdadero si la expresion es cierta
-- en todos los mundos del modelo.
cierto :: Modelo -> Exp -> Bool
cierto (K g v) e = cantidad (nodos g) == cantidad (valeEn e (K g v))
	where cantidad = foldr (+) 0

