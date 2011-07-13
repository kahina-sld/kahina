main :-
  on_exception(e,a,true).

a :-
  raise_exception(e).
a.
