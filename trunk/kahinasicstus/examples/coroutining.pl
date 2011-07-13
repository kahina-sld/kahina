:- block c(-,?), c(?,-), c(-,-).

main :-
  when(nonvar(X),a),
  when(ground(X),b),
  c(X,Y),
  when(?=(X,Y),d),
  freeze(Y,e),
  dif(X,Y),
  X=x(A), % a, c(x(A),Y)
  Y=2,    % d, e, unblock dif(X,Y)
  A=1.    % b

a.

b.

c(_,_).

d.

e.
