amico(antonio,bianca).
amico(antonio,enrico).
amico(bianca,dario).
amico(dario,carla).
amico(enrico,bianca).
amico(antonio,Chiunque).



conosce(X,Y) :- amico(X,Y) ; amico(Y,X).
conosce(X,Y) :- amico(X,Qualcuno) , amico(Qualcuno,Y).

% esempio con genitore % 

genitore(tommaso,francesca).
genitore(tommaso,vittorio).
genitore(francesca,linda).
genitore(vittorio,bianca).

nonno(X,Y) :- genitore(X,Z) , genitore(Z,Y).

% esempio 3 % 

likes(mary,food).
likes(mary,wine).
likes(jhon,beer).
likes(jhon,wine).
likes(jhon,mary).

% unificazione %

vertical(line(point(X,Y),point(X,Z))).
horizontal(line(point(X,Y),point(Z,Y))).

% procedure ricorsive % 

vicino(tavolo,penna).
vicino(penna,pc).

colore(tavolo,bianco).
colore(penna,rosso).
colore(pc,verde).
colore(X,Y) :- vicino(X,Z),colore(Z,Y).

