/* in_tree(El,T) : El occorre come etichetta in T */
/* CASO BASE: El occorre in un albero che ha El come radice, in_tree(El,t(El,_,_)) */
/* CASO RICORSIVO: El occorre in un albero se occorre nel suo sottoalbero sx o dx */ 

in_tree(El,t(El,_,_)).
in_tree(El,t(_,Left,Right)) :- in_tree(El,Left) ; in_tree(El,Right).

/* size(+T,?N): T ha N nodi */ 

size(empty,0).
size(t(_,Left,Right),H) :- size(Left,N), size(Right,M), H is N+M+1.

/* height(+T,?N): T ha altezza N */

height(empty,0).
height(t(_,Left,Right), H) :- height(Left,N), height(Right,M), (N>M, H is N+1 ; N=<M, H is M+1).

/* utilizzo di member. member = mem */ 

mem(X,[X|_]). 
mem(X,[_|Coda]) :- mem(X,Coda). 

/* append3(L1,L2,L3,Result) = Result e’ la concatenazione di L1, L2 e L3 */

append3(L1,L2,L3,Result) :- append(L1,L2,L), append(L,L3,Result).

/* backtracking */ 

max(X,Y,X) :- X>=Y.
max(X,Y,Y) :- X<Y.

sano(X) :- felice(X).
sano(X) :- ha_anticorpi(X).

felice(X) :- canta(X), balla(X).
felice(X) :- stupido(X).

ha_anticorpi(maria).

balla(linda).

canta(linda).
canta(gianni).

stupido(gianni).

/* tutti i francesci, eccetto i parigini, sono gentili */ 

gentile(X) :- francese(X), not(parigino(X)).

francese(antoine).
francese(charles).
parigino(antoine).

/* merge di liste ordinate, cut verde */ 

merge(X,[],X).
merge([],X,X).
merge([X|R1],[Y|R2],[X,Y|R]) :- X=Y, !, merge(R1,R2,R).
merge([X|R1],[Y|R2],[X|R]) :- X<Y, !, merge(R1,[Y|R2],R).
merge([X|R1],[Y|R2],[Y|R]) :- X>Y, merge([X|R1],R2,R).

/* intersezione insiemistica, cut rosso */
intersect(_,[],[]).
intersect([],_,[]).
intersect([X|Rest],Y,[X|R]) :- member(X,Y), !, intersect(Rest,Y,R).
intersect([_|Rest],Y,R) :- intersect(Rest,Y,R).


