/*esercizio 2*/
fact(0,1).
fact(X,Y) :- X>0, X1 is X-1, fact(X1,Y1), Y is X*Y1.

/*esercizio 3*/
palindroma([]).
palindroma([_]).
palindroma([X|Coda]) :- append(H,[X],Coda), palindroma(H).

/*esercizio 4*/
max(X,Y,Max) :- X>=Y, Max is X.
max(X,Y,Max) :- X<Y, Max is Y.
maxlist([X],X).
maxlist([X,Y|Coda],N) :- max(X,Y,Max), maxlist([Max|Coda],N).

/*esercizio 5*/
pari(X) :- 0 is X mod 2.
split([],[],[]).
split([X|L],[X|P],D) :- pari(X), split(L,P,D).
split([X|L],P,[X|D]) :- not(pari(X)), split(L,P,D).

/*esercizio 7*/
prefisso([],[]).
prefisso(Pre,L) :- append(Pre,_,L).

/*esercizio 8*/
suffisso([],[]).
suffisso(Suf,L) :- append(L,_,Suf).

/*esercizio 9 da errore!*/
sublist(S,L) :- append(_,S,Temp), append(Temp,_,L).

/*esercizio 10*/

rev([],[]).
rev([X],[X]).
rev([A|X],B) :- append(B2,[A],B), rev(X,B2).

del(_,[],[]):-!.
del(X,L,L) :- not(member(X,L)),!.
del(X,[X],[]) :- !.
del(X,[X|Coda],Coda2) :- del(X,Coda,Coda2),!.
del(X,[Testa|Coda],[Testa|Coda2]) :- del(X,Coda,Coda2).

subst(_,_,[],[]):-!.
subst(X,_,L,L) :- not(member(X,L)),!.
subst(X,Y,[X],[Y]):-!.
subst(X,Y,[X|Coda],[Y|Coda2]) :- subst(X,Y,Coda,Coda2).

mkset([],[]):-!.
mkset([X],[X]):-!.
mkset([X|Coda],L2):-member(X,Coda),!,mkset(Coda,L2).
mkset([X|Coda],[X|L2]):-not(member(X,Coda)),mkset(Coda,L2).

/*union([],B,B):- !.
union(A,[],A):- !.
union([X|A],B,Union) :- member(X,B),!, union(A,B,Union).
union([X|A],B,[X|Union]) :- union(A,B,Union).*/


/*esercizio 16*/
bin_height(empty,0).
bin_height(t(_,Left,Right),N):-bin_height(Left,L), bin_height(Right,R), (L>R,N is L+1; R>=L ,N is R+1).

bin_size(empty,0).
bin_size(t(_,Left,Right),N):-bin_size(Left,L),bin_size(Right,R),
(N is L+R+1).

bin_labels(empty,[]).
bin_labels(t(X,Left,Right),[X|Lista]):-bin_labels(Left,L),bin_labels(Right,R),append(L,R,Lista).

balanced(empty):-!.
balanced(t(_,empty,empty)):-!.
balanced(t(_,Left,Right)):- bin_height(Left,L),bin_height(Right,R),(L=R;L=R-1;L=R+1),balanced(Left),balanced(Right).

branch(empty,_,[]).
branch(t(X,empty,empty),X,[X]).
branch(t(Y,Left,_),X,[Y|L]):- branch(Left,X,L), L\=[].
branch(t(Y,_,Right),X,[Y|R]):- branch(Right,X,R), R\=[].

/*esercizio 18*/
arc(a,b).
arc(a,e).
arc(b,a).
arc(b,c).
arc(c,c).
arc(c,d).
arc(d,c).
arc(d,b).
arc(e,c).
path(Start,Goal,Path):-path(Start,Goal,Path,[]).
path(Start,Start,[Start],V):-not(member(Start,V)).
path(Start,Goal,[Start|Path],Visited):-not(member(Start,Visited)), arc(Start,N), path(N,Goal,Path,[Start|Visited]).


/*esame febbraio 2016 esercizio 3 su grafo (da completare)*/
arc(1,a,2).
arc(1,b,3).
arc(1,c,4).
arc(2,a,6).
arc(3,b,5).
arc(3,c,5).
arc(4,b,1).
arc(4,c,6).
arc(5,c,4).
arc(6,b,5).
actions(Start,Goal,Arcs):-actions(Start,Goal,Arcs,[]).
actions(Goal,Goal,[],[]).
actions(Start,Goal,[A],V):-not(member(Goal,V)),not(member(Start,V)),arc(Start,A,Goal).
actions(Start,Goal,[A|L],V):-not(member(Start,V)),arc(Start,A,N),actions(N,Goal,L,[Start|V]).

/*esame febbraio 2013 parte2 esercizio 3 (alberi)*/
%sottoprocedura binario(T) verifico se T è binario
%binario(empty).
binario(leaf(_)).
binario(two(_,Left,Right)):-binario(Left),binario(Right).

branch2(leaf(X),X,[X]).
branch2((two(R,L,Ri)),Leaf,[R|Branch]):-binario(two(R,L,Ri)),(branch2(L,Leaf,Branch);branch2(Ri,Leaf,Branch)).


/*luglio 2013 seconda parte esercizio 1*/
min([X,E],[Y,_],[X,E]):-X=<Y,!.
min(_,[Y,E],[Y,E]).
assocmin([],[]).
assocmin([(_,E)],E):-!.
assocmin([(N,E),(M,E1)|Coda],H):-min([N,E],[M,E1],[Min,E2]), assocmin([(Min,E2)|Coda],H).


/*settembre 2013 seconda parte esercizio 2*/
connects(a,1,2).
connects(b,1,3).
connects(c,1,4).
connects(a,2,3).
connects(a,2,5).
connects(b,2,6).
connects(c,6,7).
percorso(Start,Start,[]):-!.
percorso(Start,Dest,[D]):-connects(D,Start,Dest),!.
percorso(Start,Dest,[D|Porte]):-connects(D,Start,N),percorso(N,Dest,Porte).

/*esame febbraio 2015 esercizio 1*/
del_first(_,[],[]):-!.
del_first(X,[X|L],L):-!.
del_first(X,[A,B|L],[A|C]):-del_first(X,[B|L],C).

/*variante, elimino tutte le occorrenze*/
del2(_,[],[]):-!.
del2(X,[X|L],C):-!,del2(X,L,C).
del2(X,[A|L],[A|C]):-del2(X,L,C).

/*MergeSort*/
split2([],[],[]).
split2([X],[],[X]).
split2([],[X],[X]).
split2([X,Y|Coda],[X|U],[Y|D]):-split2(Coda,U,D).
merge([],[],[]).
merge(X,[],X).
merge([],X,X).
merge([X|Uno],[Y|Due],[X|Merge]):-X=<Y,!,merge(Uno,[Y|Due],Merge).
merge(Uno,[Y|Due],[Y|Merge]):-merge(Uno,Due,Merge).
merge_sort([],[]):-!.
merge_sort([X],[X]):-!.
merge_sort(L,Sorted):-split2(L,Uno,Due),merge_sort(Uno,U),merge_sort(Due,D),merge(U,D,Sorted),!.


/*Selection sort*/
minore([X],X).
minore([X|Coda],M):-minore(Coda,Min),M is minore(X,Min).
del_primo(_,[],[]).
del_primo(X,[X],[]).
del_primo(X,[X|Coda],Coda):-!.
del_primo(X,[A,B|Coda],[A|L]):-del_primo(X,[B|Coda],L).
sel_sort([],[]).
sel_sort([X],[X]).
sel_sort(L,[Y|Sorted]):-minore(L,Y),del_primo(Y,L,Nuova),sel_sort(Nuova,Sorted).


/*Quick sort*/
split3(_,[],[],[]).
split3(X,[Y|Coda],[Y|Sx],Dx):-X>=Y,!,split3(X,Coda,Sx,Dx).
split3(X,[Y|Coda],Sx,[Y|Dx]):-X<Y,!,split3(X,Coda,Sx,Dx).
quick_sort([],[]).
quick_sort([X],[X]).
quick_sort([X|Coda],Sorted):-split3(X,Coda,Sx,Dx),quick_sort(Sx,S),quick_sort(Dx,D),append(S,[X|D],Sorted).
