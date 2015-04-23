import Grafo
import Tipos
import Lomoba
import Parser
import Test.HUnit

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
	"parser" ~: testsParser,
	"grafo" ~: testsGrafo,
	"lomoba" ~: testsLomoba
	]

testsParser = test [
	(Var "p") 						~=? (parse "p"),
	(And (Var "p") (Var "q")) 		~=? (parse "p && q"),
	(Or (Var "p") (Var "q")) 		~=? (parse "p || q"),
	(Or (Not (Var "p")) (Var "q"))	~=? (parse "!p || q"),
	(And (D (Var "p")) (Var "q")) 	~=? (parse "<>p && q"),
	(And (B (Var "p")) (Var "q")) 	~=? (parse "[]p && q"),
	(D (And (Var "p") (Var "q"))) 	~=? (parse "<>(p && q)"),
	(B (And (Var "p") (Var "q"))) 	~=? (parse "[](p && q)")]

mockGraph1 = agNodo 2 (agNodo 1 vacio)
mockGraph2 = agNodo 4 $ agNodo 3 $ agNodo 2 $ agNodo 1 vacio
mockGraph3 = lineal [1,2,3,4]
mockGraph4 = agEje (3,5) (agEje (2,3) (agEje (2,5) (agNodo 5 (agNodo 3 (agNodo 2 vacio)))))


testsGrafo = test [
	-- Agregar nodos los agrega a la lista
	[1] ~~? nodos (agNodo 1 vacio),
	[1,2] ~~? nodos mockGraph1,
	-- Agregar nodos repetidos no afecta al grafo
	[1,2] ~~?  nodos ( agNodo 1 mockGraph1),
	-- Eliminar nodos los elimina de la lista
	[1] ~~? nodos ( sacarNodo 2 mockGraph1),
	[] ~~? nodos (sacarNodo 2 (sacarNodo 1 mockGraph1)),
	-- agEje agrega correctamente un eje
	[2] ~~? vecinos  (agEje (1, 2) mockGraph1) 1,
	-- agEje agrega correctamente y multiples vecinos
	[2,3] ~~? vecinos (agEje (1, 3) (agEje (1, 2) (agNodo 3 mockGraph1))) 1,
	[1,4] ~~? vecinos (agEje (3, 1) (agEje (3, 4) mockGraph2)) 3,
	-- lineal crea correctamente los nodos del grafo
	[1,2,3,4] ~~? nodos (lineal [1,2,3,4]),
	-- linean crea correctamente las aristas del grafo
	[2,3,4] ~~? ((vecinos mockGraph3 1) ++ (vecinos mockGraph3 2) ++ (vecinos mockGraph3 3) ++ (vecinos mockGraph3 4) ),
	-- union une correctamente los nodos de los grafos
	[1,2,3,4,5] ~~? nodos(union mockGraph3 mockGraph4),
	-- union une correctamente las aristas de los nodos,
	[2] ~~? vecinos (union mockGraph3 mockGraph4) 1,
	[3,5] ~~? vecinos (union mockGraph3 mockGraph4) 2,
	[4,5] ~~? vecinos (union mockGraph3 mockGraph4) 3,
	[] ~~? vecinos (union mockGraph3 mockGraph4) 4,
	[] ~~? vecinos (union mockGraph3 mockGraph4) 5,
	-- clasura obtiene la clasura de un grafo lineal correctamente
	[1,2,3,4] ~~? nodos (clausura mockGraph3),
	[1,2,3,4] ~~? vecinos (clausura mockGraph3) 1,
	[2,3,4] ~~? vecinos (clausura mockGraph3) 2,
	[3,4] ~~? vecinos (clausura mockGraph3) 3,
	[4] ~~? vecinos (clausura mockGraph3) 4,
	[] ~~? vecinos (clausura mockGraph3) 1234
	]
	
	
mockGraphModel1 = agEje (1,3) (agEje (1,2) (agNodo 2 (agNodo 3 (agNodo 1 vacio))))
mockRelationModel1 :: Prop -> [Mundo]
mockRelationModel1 prop = case prop of
					"p" -> [1]
					"q" -> [2, 3]
					"r" -> [3]
					_	-> []
mockModel1 = K mockGraphModel1	mockRelationModel1
	
testsLomoba = test [ 
	-- Tests de visibilidad
	0 ~=? visibilidad (parse "p"),
	1 ~=? visibilidad (parse "<>p"),
	2 ~=? visibilidad (parse "<><>p || <><>q"),
	3 ~=? visibilidad (parse "<>(<>p || <><>q)"),
	3 ~=? visibilidad (parse "[](<>p || <><>q)"),
	-- Tests de extraer
	["p", "q"] ~~? extraer (parse "[](<>p || <><>q)"),
	["p"] ~~? extraer (parse "p"),
	-- Tests de evaluacion (ejemplo del .pdf)
	True ~=? eval mockModel1 1 (parse "p && []q"),
	True ~=? eval mockModel1 1 (parse "p && <>r"),
	False ~=? eval mockModel1 1 (parse "[]r"),
	-- Tests de valeEn
	[1,2,3] ~~? valeEn (parse "q || []q") mockModel1,
	[2,3] ~~? valeEn (parse "q") mockModel1,
	[] ~~? valeEn (parse "j") mockModel1,
	[1,2,3] ~~? valeEn (parse "!<>j") mockModel1,
	-- Tests de quitar, quita correctamente del conjunto de nodos
	[1,2,3] ~~? (\(K g v) -> nodos g ) (quitar (parse "q || []q") mockModel1),
	[2,3] ~~? (\(K g v) -> nodos g ) (quitar (parse "q")  mockModel1),
	[] ~~? (\(K g v) -> nodos g ) (quitar (parse "j") mockModel1),
	-- Tests de extraer
	["q"] ~~? extraer (parse "q || []q"),
	["p", "q"] ~~? extraer (parse "(q || []q) || p"),
	-- Tests de quitar, quita correctamente de la funcion que mapea proposiciones y mundos
	[1,2,3] ~~?  valeEn (parse "q || []q") (quitar (parse "q || []q") mockModel1),
	[] ~~?  valeEn (parse "p") (quitar (parse "q") mockModel1),
	[2,3] ~~?  valeEn (parse "q") (quitar (parse "!j") mockModel1),
	[2] ~~?  valeEn (parse "q") (quitar (parse "!r") mockModel1),
	-- Tests de cierto
	True ~=? cierto mockModel1 (parse "q || []q") ,
	False ~=?  cierto mockModel1 (parse "q") ,
	False ~=?  cierto mockModel1 (parse "j") 
	]
	
	
---------------
--  helpers  --
---------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)
