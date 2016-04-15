amico(dario,federico).
amico(francesco,federico).
amico(luca,giorgio).
amico(giorgio,Chiunque).
/* definizione di un predicato */

conosce(luca,federico). 
conosce(X,Y) :- amico(X,Y).
conosce(X,Y) :- amico(X,Qualcuno) , amico(Qualcuno,Y).