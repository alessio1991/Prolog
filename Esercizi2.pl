/* ES 1: Albero genelagioco */ 
genitore(maurizio,dario).
genitore(mario,maurizio).
genitore(maurizio,valentina).
genitore(mario,massimo).
genitore(massimo,claudia).
genitore(mario,marcello).

fratello(X,Y) :- genitore(Z,X), genitore(Z,Y), not(X=Y). 
nonno(X,Y) :- genitore(X,Z), genitore(Z,Y).
zio(X,Y) :- fratello(X,Z), genitore(Z,Y).
cugino(X,Y) :- genitore(Z,X), zio(Z,Y). 
antenato(X,Y) :- genitore(X,Y). 
antenato(X,Y) :- genitore(X,Y), antenato(X,Z).

/* ES 2: fattoriale */

fact(0,1).
fact(N,X) :- N>0, N1 is N-1, fact(N1,X1), X is N*X1.

/* ES3 Palindroma */ 

palindroma([]).
palindroma([_]).
palindroma([X|Coda]) :- append(Lista,[X],Coda), palindroma(Lista).

/* Appartenenza, se un elemento è presente nella lista */ 

appartenenza(X,[X|_]).
appartenenza(X,[_|Coda]) :- appartenenza(X,Coda).

/* Verificare che due liste hanno la stessa lunghezza */

lung([],[]).
lung([T1|C1],[T2|C2]) :- lung(C1,C2).  

/* Verificare se un eleme si trova nella stessa posizione di due liste differenti */ 

pos(X,[X|_],[X|_]).
pos(X,[_|C1],[_|C2]) :- pos(X,C1,C2). 

/* Calcolare la lunghezza della lista */ 

lunghezza([],0).
lunghezza([_|Coda],N) :- lunghezza(Coda,N1), N is N1+1. 

/* Fibonacci */
fibonacci(1,1).
fibonacci(2,1).
fibonacci(N,X) :-N>1, N1 is N-1, fibonacci(N1,X1), N2 is N-2, fibonacci(N2,X2), X is X1+X2.

/* inversa di una lista */ 
inversa([],[]).
inversa([Testa|Coda],Lista) :- inversa(Coda,L2), append(L2,[Testa],Lista).

/* Verificare se un lista è crescente */
crescente([]).
crescente([_]). 
crescente([X,Y|Coda]) :- X=<Y, crescente([Y|Coda]).

/*esp(N,Esp,Ris)*/

esp(_,0,1).
esp(N,Esp,Ris) :- Esp>0, Esp1 is Esp-1, esp(N,Esp1,R), Ris is N*R.

/* Minore in un lista */

minore([X],X).
minore([Testa|Coda],X) :- minore(Coda,X1), X is min(X1,[Testa]).  

/* conta numeri */ 

somma_fino(1,1).
somma_fino(N,Ris):- N>1, N1 is N-1, somma_fino(N1,Ris1), Ris is Ris1+N.

/* Hanoi hanoi(N,Source,Dest,Appoggio)*/ 
% Se N = 1 stampo 
% Se N>1

hanoi(N) :- hanoi(N,'A','B','C').
hanoi(1,Source,Dest,App) :- !,writeln([sposto,da,Source,Dest]). 
hanoi(N,Source,Dest,App) :- N1 is N-1, hanoi(N1,Source,App,Dest), hanoi(1,Source,Dest,App), hanoi(N1,App,Dest,Source).


/* split */

pari(X) :- 0 is X mod 2.
split([],[],[]).
split([X|Coda],[X|P],D) :- pari(X),!,split(Coda,P,D).
split([X|Coda],P,[X|D]) :- split(Coda,P,D).

/* Prefisso */

pref([],[]). 
pref(Pre,L):- append(Pre,_,L). 

/* Suffisso */ 

suff([],[]).
suff(Suff,L):- append(_,Suff,L).

/* 9. Definire un predicato sublist(S,L) = S `e una sottolista di L costituita da
elementi contigui in L. Ad esempio, le sottoliste di [1,2,3] sono: la lista
vuota [] e le liste [1], [2], [3], [1,2], [2,3] e [1,2,3] stessa. */

sublist([],[]).
sublist(S,L) :- pref(S,L).
sublist(S,[T|L]) :- sublist(S,L).

/* Predicati vari */

% a) subset(?Sub,+Set) = tutti gli elementi di Sub sono anche elementi di Set.

subset([],[]).
subset([X|L],[X|S]) :- subset(L,S).
subset(L, [_|S]) :- subset(L,S).

% b) rev(+X,?Y) = Y `e la lista che contiene gli stessi elementi di X, ma in ordine inverso.

rev([],[]).
rev([T|C],L) :- rev(C,L2), append(L2,[T],L). 

% c) del_first(+X,+L,?Resto) eliminare la prima occorrenza X, trovata in L 

del_first(_,[],[]).
del_first(X,[X],[]) :- !.
del_first(X,[X|C],C):- !.
del_first(X,[A,B|C],[A|C2]) :- del_first(X,[B|C],C2), !.  

% d) del(+X,+L,?Resto) elimina tutte le occorenze trovate in L 

del(_,[],[]).
del(X,[X],[]) :- !.
del(X,[X|C],L) :- !,del(X,C,L).
del(X,[T|C],[T|L2]) :- del(X,C,L2).
/* (1,[2,1,3,1,4],X). Scorre fino in fondo la lista arrivando al caso base, in cui un lista vuota restituisce una lista vuota,
salvando L come la lista vuota, essendo [] diverso da X allora entra nell'append e riscalando 
verso indietro trava il primo elemento (4), considerando la testa 4 e la coda vuota dato che è il primo incotrato
si continua nella 4a condizione e si fa l'append (4,[],[4]). 
Si arriva a 1,4, ma verficando la 3a condizione che dice che se la testa della lista è l'elemento da elminare
allora beipassalo e continua la scansione sulla sua coda. Qua è utile il cut a sx perchè ci permette di non 
restiurci tutte le operazioni e i risultati sulla coda della 3 operazione, poichè potrebbe contenere l'occorenza 
che avevamo beipassato. 
Dopo arrivo a 3,1,4 dato che la testa non è l'occorenza la concateno direttamente a L.
Ora arrivo a 1,3,1,4 qui la testa è uguale all'occ quindi per la 3a condizione alla 4a condizione passo
la lista [3,1,4] e rifà il lavoro di del passo di prima, restituendo lo stesso risultato che non avremo in output per il cut.
Arriviamo a 2,1,3,1,4. Essendo 2 diverso dall'occorenza mettiamo la testa in L2, finendo. */


/* e) subst(+X,+Y,+L,-Nuova) = Nuova `e la lista che si ottiene da L
sostituendo tutte le occorrenze di X con Y. Se X non occorre in L,
Nuova `e uguale a L stessa. */

subst(_,_,[],[]). %se x non occorre in L, Nuova è uguale ad L stessa
subst(X, Y, [X|Coda], [Y|Queue]):- subst(X, Y, Coda, Queue),!.
subst(X, Y, [Inizio|Coda], [Inizio|Resto]):- subst(X, Y, Coda, Resto).

/* f) mkset(+L,-Set) = Set `e una lista senza ripetizioni che contiene
tutti e solo gli elementi di L (senza utilizzare il predicato predefinito
list to set/2).*/

mkset([],[]).
mkset([X|C1],[X|C2]) :- del(X,C1,Ris), mkset(Ris,C2).


mkset1([],[]).
mkset1([X|Rest],Set):- member(X,Rest), mkset1(Rest,Set).
mkset1([X|Rest],[X|Set]):- not(member(X,Rest)),mkset1(Rest,Set).
/* (g) union(+A,+B,-Union) = Union `e una lista (senza ripetizioni, se
anche A e B sono senza ripetizioni) che rappresenta l’unione di A e
B. */

union([],B,B):- !.
union(A,[],A):- !.
union([X|A],B,Union) :- member(X,B),!, union(A,B,Union).
union([X|A],B,[X|Union]) :- union(A,B,Union).

/* Fibonacci con assert */ 

:- dynamic(fib1/2). 

fib1(1,1).
fib1(2,1).
fib1(N,F) :- N>2, N1 is N-1, fib1(N1,F1), N2 is N-2, fib1(N2,F2), F is F1+F2, asserta(fib1(N,F)). %memorizza il risultato

/* cancellazione dei fatti di fib1 con N>2 */ 

cancella :- clause(fib1(N,F),true), N>2, retract(fib1(N,F)), fail. 
cancella. 

/* intersezione di insiemi */ 
%con findall
intersect(S1,S2,Ris) :- findall(X, (member(X,S1),member(X,S2)), Ris).

%metodo ricorsivo

inters(_,[],[]).
inters([],_,[]).
inters([X|Coda],Y,[X|Resto]) :- member(X,Y),!, inters(Coda,Y,Resto).
inters([_|Coda],Y,Resto) :- inters(Coda,Y,Resto).

/* Occours_in */

occours_in(X,Y) :- X=Y ; aux(X,Y).
aux(X,[Y|Rest]) :- occours_in(X,Y) ; aux(X,Rest).

/* Definire un predicato flat(+X,?Y) che riporti in Y tutti i termini
(atomi o strutture) che occorrono in X. */ 

flat([],[]).
flat([X|Coda],Flat) :- flat(X,Fl), flat(Coda,Fl2), append(Fl,Fl2,Flat).
flat([T|Coda],[T|Coda2]) :- not(is_list(T)), flat(Coda,Coda2). 

flat1([],[]).
flat1([X|_],Y):- is_list(X),!,flat1(X,Y).
flat1([X|Coda],[X|Resto]):- flat1(Coda,Resto).

/* 13)Definire un predicato insert(X,L1,L2), vero se L2 si ottiene inserendo X
in L1 (in qualsiasi posizione). Almeno una delle due liste L1 e L2 devono
essere istanziate. */

insert(X,L1,[X|L1]).
insert(X,[Testa|L1],[Testa|L2]) :- insert(X,L1,L2).

/* 14)Definire un predicato permut(X,Y) vero se X e Y sono liste e Y `e una
permutazione di X (senza usare il predicato predefinito permutation/2). */

permut([],[]).
permut([X|Coda], Permut) :- permut(Coda,P), insert(X,P,Permut).

/* 15) Definire un predicato search subset(+IntList,+N,?Set), dove IntList
`e una lista di interi positivi e N un intero positivo, che sia vero se Set
`e una lista rappresentante un sottoinsieme di IntList, tale che la somma
degli elementi in Set `e uguale a N. Si pu`o assumere che IntList sia senza
ripetizioni. */ 
%subset
/*subset([],[]).
subset([Testa|Sub],[Testa|Set]) :- subset(Sub, Set).
subset(Sub,[_|Set]) :- subset(Sub, Set).*/

somma_elem([],0).
somma_elem([X|Coda],Ris) :- somma_elem(Coda,Ris1), Ris is X+Ris1.

search_subset(Lista,N,Set) :- subset(Set,Lista), somma_elem(Set,N). 

/* con backtracking */

search_subset2(_,0,[]):- !.
search_subset2([Head|List],N,[Head|Subset]) :- N1 is N-Head, N1 >= 0, search_subset2(List,N1,Subset).
search_subset2([_|List],N,Subset) :- search_subset2(List,N,Subset).

/* ALBERI */ 

/* Definire il metodo teste is_ntree(+Tree) per verificare se Tree è un albero n-ario*/

/*is_ntree(_ =>+ Tlist) :- !,length(Tlist,N), N>0, is_ntreelist(TList). % N>0 almeno un sottoalbero
is_ntree(_). % caso foglia

is_ntreelist([]).
is_ntreelist([T|TList]) :- is_ntree(T), is_ntreelist(TList).*/

/* Definire il metodo per la verifica di un albero BINARIO */ 

is_tree(empty).
is_tree(t(_,Left,Right)) :- is_tree(Left), is_tree(Right).

/* Definire in_tree(El,T) per verificare che El occorre come etichetta di qualche nodo dell'albero T */

in_tree(El,t(El,_,_)). %caso base, El occorre come etichetta della radice dell'albero
in_tree(El,t(_,Left,Right)) :- in_tree(El,Left) ; in_tree(El,Right). 

/* 16 esercizi su alberi */ 

/* a) bin height(+T,?N) = N `e l’altezza dell’albero T. Il predicato fallisce
se T `e l’albero vuoto. */

bin_height(empty,0).
bin_height(t(_,Left,Right),H) :- bin_height(Left,H1), bin_height(Right,H2), (H1>H2, H is H1+1 ; H1=<H2, H is H2+1). 

/* b) reflect(T,T1) = T `e l’immagine riflessa di T1. Almeno uno tra T e T1 
devono essere completamente istanziati. */

reflect(t(Root,empty,empty),t2(Root,empty,empty)). 
reflect(t(Root,Left,Right),t2(Root,Right,Left)).

/* c) bin size(+T,?N) = N `e il numero di nodi dell’albero T. */

bin_size(empty,0).
bin_size(t(_,Left,Right),N) :- bin_size(Left,N1), bin_size(Right,N2), N is N1+N2+1.

/* d) bin labels(+T,-L)= L `e una lista di tutte le etichette dei nodi di
T. Se diversi nodi di T hanno la stessa etichetta, la lista L conterr`a
ripetizioni dello stesso elemento. Gli elementi di L possono occorrere
in qualsiasi ordine. */

bin_label(empty,[]).
bin_label(t(Root,Left,Right),[Root|Label]):- bin_label(Left,Coda), bin_label(Right,Coda2), append(Coda,Coda2,Label).

/* e) balanced(+T) = l’albero T `e bilanciato (un albero `e bilanciato
se per ogni nodo n, le altezze dei sottoalberi sinistro e destro di n
differiscono al massimo di 1).*/

balanced(empty).
balanced(t(_,Left,Right)) :- balanced(Left), balanced(Right), bin_height(Left,X), bin_height(Right,Y), Z is abs(X-Y), Z=<1.

/* f) branch(+T,?Leaf,?Path) = Path `e una lista che rappresenta un
ramo dalla radice di T fino a una foglia etichettata da Leaf. */

branch(empty,_,[]).
branch(t(X,_,_),X,[X]).
branch(t(Y,Left,Right),X,[Y|L]):- branch(Left,X,L), L\=[].
branch(t(Y,Left,Right),X,[Y|R]):- branch(Right,X,R), R\=[].

/* Alberi n-ari, forma Root => Subtrees (albero con radice Root e sottoalberi nella lista non vuota Subtrees)*/ 

:- op(600,xfx,=>).

labels(X,[X]).
labels(X => Figli,[X|Rest]) :- !,labels_figli(Figli,Rest).

labels_figli([],[]).
labels_figli([A|Rest], Labels) :- labels(A,LA), labels_figli(Rest,LF), append(LA,LF,Labels).

/* (a) height(+T,?N) = N `e l’altezza dell’albero T */

height(empty, 0).
height(_,Subtrees,N) :- length(Subtrees,M), N is M+1. 


/* 18) Un grafo si pu`o rappresentare mediante un insieme di fatti della forma
arc(X,Y), che definiscono la relazione binaria “esiste un arco da X a Y”.
Definire un predicato path(?Start,?Goal,?Path) = Path `e una lista che
rappresenta un cammino da Start a Goal nel grafo definito nel programma.
Suggerimento: utilizzare un predicato ausiliario a quattro argomenti 
path(?Start,?Goal,?Path,+Visited)= Path `e una lista che rappresenta
un cammino da Start a Goal che non passa per nessuno dei nodi della lista
Visited. */

arc(a,b).
arc(a,e).
arc(b,a).
arc(b,c).
arc(c,c).
arc(c,d).
arc(d,c).
arc(d,b).
arc(e,c).

path(S,G,P):- path(S,G,P,[]).
path(S,S,[S|_]).
path(S,G,[S|P],Visited):- not(member(S,Visited)), arc(S,X), path(X,G,P,[S|Visited]). 






