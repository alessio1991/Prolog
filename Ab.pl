fact(0,1).
fact(X,N) :- X>0, X1 is X-1, fact(X1,N1), N is N1*X.

palindroma([]).
palindroma([X]).
palindroma([X|Coda]):- append(Rest,[X],Coda), palindroma(Rest).

maxlist([X],X).
maxlist([X|Coda],N) :- maxlist(Coda,N1), N is max(X,N1).

pari(X) :- 0 is X mod 2.

split([],[],[]).
split([X|Coda],[X|P],D):- pari(X),!,split(Coda,P,D).
split([X|Coda],P,[X|D]):- split(Coda,P,D).

prefisso([],[]).
prefisso(Lista,Pre) :- append(Pre,_,Lista).

suffisso([],[]).
suffisso(Lista,Sub) :- append(_,Sub,Lista).

sublist([],[]).
sublist(Lista,S):- prefisso(Lista,S).
sublist([_|Lista],S) :- sublist(Lista,S).

subset([],[]).
subset([X|L],[X|Set]) :- subset(L,Set).
subset([_|L],Set) :- subset(L,Set).

rev([],[]).
rev([X],[X]).
rev([X|L],R) :- rev(L,R2), append(R2,[X],R),!.

del_first(_,[],[]).
del_first(X,[X],[]).
del_first(X,[X|Lista],Lista):-!. 
del_first(X,[A,B|Lista],[A|Lista2]):- del_first(X,[B|Lista],Lista2).

del(_,[],[]).
del(X,[X],[]):- !.
del(X,[X|Coda],Lista) :- !, del(X,Coda,Lista).
del(X,[T|Coda],[T|Lista]) :- !, del(X,Coda,Lista). 


subst(_,_,[],[]).
subst(X,Y,[X],[Y]):-!.
subst(X,Y,[X|Lista],[Y|Lista2]):- subst(X,Y,Lista,Lista2),!.
subst(X,Y,[T|Lista],[T|Lista2]) :- subst(X,Y,Lista,Lista2).

mkset([],[]).
mkset([X],[X]).
mkset([X|Lista],Lista2):- member(X,Lista),mkset(Lista,Lista2),!.
mkset([X|Lista],[X|Lista2]) :- not(member(X,Lista)), mkset(Lista,Lista2),!.

union([],B,B).
union(A,[],A).
union([X|A],B,Union) :- member(X,B),!,union(A,B,Union).
union([X|A],B,[X|Union]) :- union(A,B,Union).

occurs_in(X,Y) :- X=Y; aux(X,Y).
aux(X,[Y|Rest]) :- occurs_in(X,Y) ; aux(X,Rest).

flat([],[]).
flat([X|_],Lista) :- is_list(X),!, flat(X,Lista).
flat([X|Resto],[X|Lista]) :- flat(Resto,Lista).


pair(_,[],[]).
pair(X,[Y|A],Lista):- append([X],[Y],L), pair(X,A,L2), append(L,L2,Lista).
cartprod([],_,[]).
cartprod([X|A],B,Set) :- cartprod(A,B,AxB), pair(X,B,Lista), append(Lista,AxB,Set).


insert(X,L1,[X|L1]).
insert(X,[T|L1],[T|L2]) :- insert(X,L1,L2).

permut([],[]).
permut([X|Coda],Perm) :- permut(Coda,P), insert(X,P,Perm).


somma_el([],0).
somma_el([X],X).
somma_el([X|Lista],N) :- somma_el(Lista,N1), N is N1+X,!.

search_subset([],_,[]).
search_subset(_,0,[]).
search_subset(Lista,N,Ris) :- subset(Lista,Ris), somma_el(Ris,N). 

appartenenza(El,[El|Lista]).
appartenenza(El,[_|Lista]) :- appartenenza(El,Lista).

fib(1,1).
fib(2,1).
fib(N,X) :- N>2, N1 is N-1, fib(N1,X1), N2 is N1-1, fib(N2,X2), X is X1+X2.

lunghezza([],0).
lunghezza([X|Lista],N):- lunghezza(Lista,N1), N is N1+1.

stessa_lung([],[]).
stessa_lung([_|L1],[_|L2]) :- stessa_lung(L1,L2).

stessa_pos(X,[X|_],[X|_]).
stessa_pos(X,[_|Coda1],[_|Coda2]):- stessa_pos(X,Coda1,Coda2).

bin_heigth(empty,0).
bin_heigth(t(_,Left,Rigth),H) :- bin_heigth(Left,H1), bin_heigth(Rigth,H2), (H1>H2,H is H1+1;H1=<H2,H is H2+1).

reflect(t(Root,empty,empty),t2(Root,empty,empty)).
reflect(t(Root,Left,Rigth),t2(Root,Rigth,Left)).

bin_size(empty,0).
bin_size(t(_,Left,Rigth),N) :- bin_size(Left,N1), bin_size(Rigth,N2), N is N1+N2+1.

balanced(empty).
balanced(t(_,Left,Rigth)) :- balanced(Left), balanced(Rigth), bin_heigth(Left,Nl), bin_heigth(Rigth,Nr), Z is abs(Nl-Nr), Z=< 1.

branch(empty,_,[]).
branch(t(X,empty,empty),X,[X]):-!.
branch(t(Root,Left,Rigth),X,[Root|ListaL]) :- branch(Left,X,ListaL), ListaL\=[].
branch(t(Root,Left,Rigth),X,[Root|ListaR]) :- branch(Rigth,X,ListaR), ListaR\=[].

/* mergesort */

split2([],[],[]).
split2([X],[X],[]).
split2([X,Y|Lista],[X|Sx],[Y|Dx]) :- split2(Lista,Sx,Dx).

merge([],L,L).
merge(L,[],L).
merge([X|Uno],[Y|Due],[X|Merge]) :- X=<Y,!, merge(Uno,[Y|Due],Merge).
merge(Uno,[Y|Due],[Y|Merge]) :- merge(Uno,Due,Merge).

merge_sort([],[]):-!.
merge_sort([X],[X]):-!.
merge_sort(Lista,Sorted) :- split2(Lista,Sx,Dx), merge_sort(Sx,Ssorted), merge_sort(Dx,Dsorted), merge(Ssorted,Dsorted,Sorted),!.


/* selectionsort */

minore2([X],X).
minore2([X|Lista],Min) :- minore2(Lista,M), Min is min(X,M).

del_first2(_,[],[]).
del_first2(X,[X|Lista],Lista):- !.
del_first2(X,[A,B|Lista],[A|Lista2]) :- del_first(X,[B|Lista],Lista2).

sel_sort([],[]).
sel_sort([X],[X]).
sel_sort(Lista,[Min|Sorted]) :- minore2(Lista,Min), del_first2(Min,Lista,Nuova), sel_sort(Nuova,Sorted),!. 

/* quicksort */ 

split3(_,[],[],[]).
split3(X,[Y|Coda],Sx,[Y|Dx]):- X<Y,!,split3(X,Coda,Sx,Dx).
split3(X,[Y|Coda],[Y|Sx],Dx) :- X>=Y,!,split3(X,Coda,Sx,Dx).

quick_sort([],[]).
quick_sort([X|Lista],Sorted) :- split3(X,Lista,Sx,Dx), quick_sort(Sx,Ssx), quick_sort(Dx,Sdx), append(Ssx,[X|Sdx],Sorted).


conta_el(_,[],0).
conta_el(X,[X|Lista],N) :- conta_el(X,Lista,N1), N is N1+1,!.
conta_el(X,[_|Lista],N) :- conta_el(X,Lista,N).

conta([],[]).
conta([X|Coda],Lista):- conta_el(X,[X|Coda],N), append([(X,N)],[],App), delete(Coda,X,Coda2), conta(Coda2,Lista2), append(App,Lista2,Lista),!.
conta([_|Coda],Lista):- conta_el(Coda,Lista).


minbranch(t(X,empty,empty),[X],X).
minbranch(t(Root,Left,empty),[Root|List],Cost) :- minbranch(Left,List,CostoL), Cost is CostoL+Root.
minbranch(t(Root,empty,Rigth),[Root|List],Cost) :- minbranch(Rigth,List,CostoR), Cost is CostoR+Root.
minbranch(t(Root,Left,Rigth),[Root|ListL],Cost) :- minbranch(Left,ListL,CostoL), minbranch(Rigth,ListR,CostoR), CostoL<CostoR, Cost is CostoL+Root,!.
minbranch(t(Root,Left,Rigth),[Root|ListR],Cost) :- minbranch(Left,ListL,CostoL), minbranch(Rigth,ListR,CostoR), CostoR=<CostoL, Cost is CostoR+Root,!.


branch2(empty,_,[]).
branch2(t(Root,empty,empty),Root,[Root]).
branch2(t(Root,Left,Rigth),X,[Root|ListL]) :- branch2(Left,X,ListL), ListL\=[].
branch2(t(Root,Left,Rigth),X,[Root|ListR]) :- branch2(Rigth,X,ListR), ListR\=[].

crescente([],[]).
crescente([X],[X]).
crescente([X,Y|Lista],[X|Lista2]) :- crescente([Y|Lista],Lista2), Y>=X.

branch3(empty,_,[]).
branch3(t(Root,empty,empty),[Root]).
branch3(t(Root,Left,Rigth),[Root|ListL]) :- branch3(Left,ListL).
branch3(t(Root,Left,Rigth),[Root|ListR]) :- branch3(Rigth,ListR).

ordered(T,Branch) :- branch3(T,B), crescente(B,Branch).

cresc([]).
cresc([_]).
cresc([X,Y|List]) :- cresc([Y|List]), Y>=X,!.
