main :-
  findall(X,a(X),_).

a(X) :-
  b(X),
  c(X).

b(1).
b(2).
b(3).

c(1) :-
  false.
c(2) :-
  d.
c(3) :-
  d.

d.
