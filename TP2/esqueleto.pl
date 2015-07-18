%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(N,M,T) :- length(T, N), freeVarLists(M, T).

%% freeVarLists(+Length, -List)
freeVarLists(_,[]).
freeVarLists(Length, [X|XS]) :- length(X, Length), freeVarLists(Length, XS).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(Y,X), T) :- nth0(Y, T, Row), nth0(X, Row, ocupada).


%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
% Vecino izquierdo
vecino(pos(Y,X),T,PosVecino) :- XVecino is X-1,
	onBounds(XVecino, Y, T),
	PosVecino = pos(Y,XVecino).
%% Vecino superior
vecino(pos(Y,X),T,PosVecino) :- YVecino is Y-1,
	onBounds(X, YVecino, T),
	PosVecino = pos(YVecino, X).
%% Vecino derecho
vecino(pos(Y,X),T,PosVecino) :- XVecino is X+1,
	onBounds(XVecino, Y, T),
	PosVecino = pos(Y,XVecino).
%% Vecino inferior
vecino(pos(Y,X),T,PosVecino) :- YVecino is Y+1,
	onBounds(X, YVecino, T),
	PosVecino = pos(YVecino, X).

%% onBounds(+X,+Y,+T)
onBounds(X,Y,T) :- rows(T,YLimit), columns(T,XLimit), X >= 0, Y >= 0, X < XLimit, Y < YLimit.

%% rows(+Tablero, -Filas)
rows(T,Rows) :- length(T, Rows).
%% columns(+Tablero, -Columnas)
columns(T,Columns) :-  nth0(0, T, FirstRow), length(FirstRow, Columns).


%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(Pos,T,Vecino) :- vecino(Pos,T,Vecino), celda(Vecino,T,C), var(C).

%% celda(+Pos, +Tablero, -Elemento)
celda(pos(Y,X),T,E) :- nth0(Y, T, Row), nth0(X, Row, E).



%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

camino(I,F,T,C) :- I = pos(Y1,X1),
	F = pos(Y2,X2),
	onBounds(X1,Y1,T),
	onBounds(X2,Y2,T),
	caminoAux(I,F,T,C,[]).

caminoAux(Pos,Pos,T,C,Aux) :- not(member(Pos, Aux)),
	Pos = pos(Y,X),
	onBounds(X,Y,T),
	C = [Pos].

caminoAux(I,F,T,C,Aux) :- not(member(I,Aux)),
	vecinoLibre(I,T,NewI),
	append([I], NewPath, C) ,
	caminoAux(NewI,F,T,NewPath, [I|Aux]).


%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(I,F,T,N) :- count(camino(I,F,T,_), N).
count(P,Count) :- findall(1,P,L), length(L,Count).

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(I,F,T,C):- I = pos(Y1,X1),
	F = pos(Y2,X2),
	onBounds(X1,Y1,T),
	onBounds(X2,Y2,T),
	camino2Aux(I,F,T,C,[]).

%% camino2Aux(+Inicio, +Fin, +Tablero, -Camino, +Visitados)
camino2Aux(Pos,Pos,T,C,Aux) :- not(member(Pos, Aux)),
	Pos = pos(Y,X),
	onBounds(X,Y,T),
	C = [Pos].

camino2Aux(I,F,T,C,Aux) :- not(member(I,Aux)),
	findall(Pos,(vecinoLibre(I,T,Pos),
	     not(member(Pos,Aux))),VecinosLibres),
	manhattanOrdered(F,VecinosLibres,VecinosOrdenados),
	append([I], NewPath, C),
	camino2AuxVecinos(VecinosOrdenados,F,T,NewPath,[I|Aux]).

%% camino2AuxVecinos(+Vecinos, +Fin, +Tablero, -Camino, +Visitados)
camino2AuxVecinos([],_,_,_,_) :- false.
camino2AuxVecinos([V|_],F,T,C,Aux) :- camino2Aux(V,F,T,C,Aux).
camino2AuxVecinos([_|Vs],F,T,C,Aux) :- camino2AuxVecinos(Vs,F,T,C,Aux).



%manhattanOrdered(+Final,+Vecinos,-VecinosOrdenados)
manhattanOrdered(F,Vs,Os) :- applyDistanceMask(F,Vs,Ms),
	predsort(manhattanCompare,Ms,OrderedMs),
	removeDistanceMask(OrderedMs,Os).

%%manhattanOrdered(+Pos1,+Pos2,-ManhattanDistance)
manhattanDistance(pos(Y1, X1), pos(Y2,X2), Distance) :-
	Distance is (abs(Y2-Y1) + abs(X2-X1)).

%%applyDistanceMask(+Destino,+Posiciones,-PosicionesYDistancia)
applyDistanceMask(_,[],D):- D = [].
applyDistanceMask(F,[P|Ps],D) :- D = [manhattan(P,MDistance) | Ms],
	manhattanDistance(P,F,MDistance),
	applyDistanceMask(F,Ps,Ms).

%%removeDistanceMask(+PosicionesYDistancias, -Posiciones)
removeDistanceMask([],Pos) :- Pos = [].
removeDistanceMask([manhattan(P,_) | Ms], Pos) :- Pos = [P | Ps],
	removeDistanceMask(Ms,Ps).

%%manhattanCompare(-Order, +PosYDist1, +PosYDist2)
manhattanCompare(>, manhattan(_,D1), manhattan(_,D2)) :-
	D1 > D2.
manhattanCompare(<, manhattan(_,D1), manhattan(_,D2)) :-
	not(D1 > D2).
manhattanCompare(=, manhattan(_,_), manhattan(_,_)) :-
	false.


%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.
camino3(_,_,_,_).

%%camino3Aux(Pos,Pos,T,C,Aux) :- not(member(Pos, Aux)),
%%	Pos = pos(Y,X),
%%	onBounds(X,Y,T),
%%	C = [Pos].

%%camino3Aux(I,F,T,C,Aux) :- not(member(I,Aux)),
%%	findall(Pos,(vecinoLibre(I,T,Pos),
%%		     not(member(Pos,Aux))),VecinosLibres),

%%	append([I], NewPath, C),
%%	camino2AuxVecinos(VecinosLibres,F,T,NewPath,[I|Aux]).

%% camino2AuxVecinos(+Vecinos, +Fin, +Tablero, -Camino, +Visitados)
%%camino3AuxVecinos([],_,_,_,_) :- false.
%%camino3AuxVecinos([V|_],F,T,C,Aux) :- camino2Aux(V,F,T,C,Aux).
%% camino3AuxVecinos([_|Vs],F,T,C,Aux) :-
%% camino2AuxVecinos(Vs,F,T,C,Aux).



camino3(I,F,T,C) :- I = pos(Y1,X1),
	F = pos(Y2,X2),
	onBounds(X1,Y1,T),
	onBounds(X2,Y2,T),
	camino3Aux(I,F,T,C,[]).

camino3Aux(Pos,Pos,T,C,Aux) :- not(member(Pos, Aux)),
	Pos = pos(Y,X),
	onBounds(X,Y,T),
	C = [Pos]
	assert(minPath(Pos,0)).

camino3Aux(I,F,T,C,Aux) :- not(member(I,Aux)),
	vecinoLibre(I,T,NewI),
	append([I], NewPath, C) ,
	camino3Aux(NewI,F,T,NewPath, [I|Aux]),
	minPath(NewI, Distance),
	NewDistance is 1 + Distance,
	shortestPath(I,NewDistance),
	assert(minPath(I,NewDistance)).
		


shortestPath(P,D) :- not((minPath(P,D2), D2 < D)).

:- dynamic minPath/2.
minPath(pos(0,0),0).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(I,F,T1,T2,C) :- camino(I,F,T1,C), camino(I,F,T2,C).


%%Tableros de prueba
tablero(ej2x2Libre, T) :- tablero(2,2,T).

tablero(ej3x3MedioOcupado, T) :- tablero(3,3,T),
	ocupar(pos(1,1),T).

tablero(ej5x5Catedra, T) :- tablero(5, 5, T),
	ocupar(pos(1, 1), T),
	ocupar(pos(1, 2), T).

test(1) :- manhattanDistance(pos(0,0), pos(5,0), Distance),
	Distance = 5.

test(2) :- F = pos(0,0), P1 = pos(5,0), P2 = pos(0,4),
	applyDistanceMask(F, [P1, P2], MaskedDistance),
	MaskedDistance = [manhattan(P1,5), manhattan(P2,4)].

test(3) :- F = pos(0,0), P1 = pos(5,0), P2 = pos(0,4),
	applyDistanceMask(F, [P1, P2], MaskedDistance),
	removeDistanceMask(MaskedDistance, Pos),
	Pos = [P1,P2].

test(4) :- F = pos(0,0), P1 = pos(5,0), P2 = pos(0,4), P3 = pos(0,1),
	P4 = pos(7,4), P5 = pos(4,0),
	manhattanOrdered(F, [P1, P2, P3, P4, P5], Ordered),
	Ordered = [P3,P2,P5,P1,P4].

test(5) :- I = pos(0,0), F = pos(1,0), tablero(ej2x2Libre, T),
	findall(C, camino(I,F,T,C), Cs),
	Cs = [[pos(0, 0), pos(0, 1), pos(1, 1), pos(1, 0)],
	      [pos(0, 0), pos(1, 0)]].

test(6) :- I = pos(0,0), F = pos(1,0), tablero(ej2x2Libre, T),
	findall(C, camino2(I,F,T,C), Cs),
	Cs = [[pos(0, 0), pos(1, 0)],
	      [pos(0, 0), pos(0, 1), pos(1, 1), pos(1, 0)]].

test(7) :- I = pos(0,0), F = pos(2,3), tablero(ej5x5Catedra, T),
	cantidadDeCaminos(I,F,T,N), N = 287.


tests :- forall(between(1, 7, N), test(N)).
