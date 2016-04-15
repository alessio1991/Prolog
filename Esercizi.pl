/* ES 1: Albero genelagioco */ 
genitore(maurizio,dario).
genitore(mario,maurizio).
genitore(maurizio,valentina).
genitore(mario,massimo).
genitore(massimo,claudia).
genitore(mario,marcello).

nonno(X,Y) :- genitore(X,Z), genitore(Z,Y).
fratello(X,Y) :- genitore(Z,X), genitore(Z,Y), not(X=Y).
zio(X,Y) :- genitore(Z,Y), fratello(Z,X). 
cugino(X,Y) :- genitore(Z,X), zio(Z,Y). 
antenato(X,Y) :- genitore(X,Y).
antenato(X,Y) :- genitore(X,Y) , antenato(X,Z).
discendente(X,Y) :- genitore(Y,X) ; nonno(Y,X).  

/* ES 2: Definire un predicato fact(+X,?Y), vero se Y `e il fattoriale di X. */
fact(0,1). 
fact(X,Y) :- X>0, X1 is X-1, fact(X1,Y1), Y is X*Y1.

/* ES 3: 
Definire il predicato palindroma(X), vero se X `e una lista palindroma
(se la lista viene letta in un verso o nell’altro si ottiene la stessa sequenza
di elementi). Ad esempio [a,b,c,b,a] `e palindroma, [a,b,c,a] non lo
`e */

palindroma([]). /*Una lista vuota è palindroma*/ 
palindroma([_]). /*Una lista di un elemento è palidroma*/
palindroma([Testa|List]) :- append(Lista,[Testa],List), palindroma(Lista). /* utilizzando append, che concatena le parti in una lista,
                                                                            mi faccio restituire quello che manca per formare List, da 
                                                                            una lista di supporto, partendo da TESTA. Poi faccio palindroma su Lista
                                                                            fino al caso base, lista di un solo elemento. */
/* con metodo reverse (fatto da noi) */ 
rev([],[]).
rev([X],[X]).
rev([X|Coda],Y) :- rev(Coda,Lista), append(Lista,[X],Y).
palindroma2(X) :- rev(X,X).

/* ES 4:
. Definire un predicato maxlist(+L,?N) (dove L `e una lista di numeri),
vero se N e’ il massimo elemento della lista L. Fallisce se L `e vuota.
*/

max(X,Y,Max) :- X>=Y, Max is X.
max(X,Y,Max) :- X<Y, Max is Y. 
maxlist([X],X).
maxlist([X,Y|List],N) :- max(X,Y,Max), maxlist([Max|List],N).

maxlist2([X],X).
maxlist2([X|R],N) :- maxlist(R,Max), N is max(X,Max).  

/* ES 5: 
Avendo definito
pari(X) :- 0 is X mod 2.
definire il predicato split(+L,?P,?D) = se L `e una lista di interi, P `e la
lista contenente tutti gli elementi pari di L e D tutti quelli dispari (nello
stesso ordine in cui occorrono in L).
*/

pari(X) :- 0 is X mod 2.
split([],[],[]).
split([X|L],[X|P],D) :- pari(X),!,split(L,P,D).  /* si fa la verifica sulla testa della Lista inserita, e se pari va inserita nella lista Pari altrimenti in Dispari */
split([X|L],P,[X|D]) :- split(L,P,D).

/* ES 6: Hanoi */

move(1,X,Y,_) :- write('Move top disk from'), write(X), write('to'), write(Y), nl.
move(N,X,Y,Z) :- N>1, M is N-1, move(M,X,Z,Y), move(1,X,Y,_), move(M,Z,Y,X). 

/* 7. Definire un predicato prefisso(Pre,L) = la lista Pre `e un prefisso della
lista L. Ad esempio, i prefissi della lista [1,2,3] sono: la lista vuota [] e
le liste [1], [1,2] e [1,2,3] stessa. */

pref([],[]).
pref(Pre,L) :- append(Pre,_,L).

/* 8. Definire suffisso. */

suff([],[]). 
suff(Suff,L) :- append(L,_,Suff). 

/* 9. Sottolista  quando conca due cose, ottengo un Temp (Tmp), devo dire che il Tmp concatenato con qualcos'altro da list */ 

sublist(Sub,List) :- append(_,Sub,Tmp) , append (Tmp,_,List). 

/* 10. Definire i predicati */ 

/* unione di due liste */ 
union([],A,A).
union(A,[],A). 
union([X|A],B,Union) :- member(X,B), !, union(A,B,Union). /* se X non appartiene a B calcola l unione di B, e al ritorno della ricorsione metti X in testa */
union([X|A],B,[X|Union]) :- union(A,B,Union). 


/* 12.  Prodotto cartesiano */ 

/* pair(X,B,Pairs) = Pairs è la lista di (x,b) per b in B */

cartprod([],_,[]).
cartprod([X|A],B,CP) :- cartprod(A,B,AxB), pair(X,B,Nuove), append(Nuove,AxB,CP). 