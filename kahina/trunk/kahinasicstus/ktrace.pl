:- module(ktrace,[ktrace/0,
                  noktrace/0]).

:- use_module(kahinasicstus).

:- dynamic kbreakpoint/1.

ktrace :-
  \+ kbreakpoint(_),
  add_breakpoint([\+module(ktrace),\+pred(noktrace/0)]-[kahina_breakpoint_action],Breakpoint),
  assert(kbreakpoint(Breakpoint)).

noktrace :-
  retract(kbreakpoint(Breakpoint)),
  remove_breakpoints([Breakpoint]).
