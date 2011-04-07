:- module(ktrace,[ktrace/0,
                  noktrace/0]).

:- use_module(kahinasicstus).

:- dynamic kbreakpoint/1.

ktrace :-
  \+ kbreakpoint(_),
  add_breakpoint([]-[kahina_breakpoint_action],Breakpoint), % TODO This isn't really it... creates a separate trace for every goal?
  assert(kbreakpoint(Breakpoint)).

noktrace :-
  retract(kbreakpoint(Breakpoint)),
  remove_breakpoints([Breakpoint]).
