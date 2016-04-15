/* Febbraio 2011 */ 

conta_el(_,[],0).
conta_el(X,[T|C],N) :- X==T->conta_el(X,C,Y), N is Y+1; conta_el(X,C,Y), N is Y. 