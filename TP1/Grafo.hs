module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
	show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
vacio :: Grafo a
vacio = G [] (\x -> [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos (G ns r) = ns

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos (G ns r) n = r n

-- Ejercicio 4
agNodo :: Eq a => a -> Grafo  a -> Grafo a
agNodo n (G ns r) = G ( if elem n ns then ns else n:ns ) r

-- Ejercicio 5
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo n (G ns r) = G (quitar n ns) (\x -> quitar n (r x))
						where quitar n = filter (/=n)

-- Ejercicio 6
agEje :: Eq a => (a,a) -> Grafo a -> Grafo a
agEje (n1,n2) (G ns r) = G ns (if (elem n1 ns && elem n2 ns)then (\x -> agEnN1 x n1 n2) else r)
						where agEnN1 x n1 n2 = if x==n1 then (agN2SiNoEsta n2 (r x)) else (r x)
							where agN2SiNoEsta n ns = if elem n ns then ns else n:ns

-- Ejercicio 7
lineal :: Eq a => [a] -> Grafo a
lineal xs = foldr2 (curry(agEje)) (foldr agNodo vacio xs) (init xs) (tail xs)

-- Ejercicio 8
union :: Eq a => Grafo a -> Grafo a -> Grafo a
union (G ns1 r1) (G ns2 r2) = foldr agEje (foldr agNodo (G ns1 r1) ns2) (obtenerListaDeEjesR2) 
								where obtenerListaDeEjesR2 = foldr (++) [] (listasDeListas)
									where listasDeListas = foldr (\x c -> (obtenerVecinos x):c) [] ns2 -- listasDeListas tiene una lista de nodos que estan en ns2, en donde cada nodo tiene una lista de tuplas de él con cada uno de sus vecinos 
										where obtenerVecinos x = foldr (\y b -> (x,y):b) [] (r2 x) -- armo lista con la tupla mencionada en el comentario de arriba

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined



-- Aux
foldr2:: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 f b [] [] = b
foldr2 f b (x:xs) (y:ys) = f x y (foldr2 f b xs ys)