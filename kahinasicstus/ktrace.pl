:- module(ktrace,[ktrace/0,
                  noktrace/0]).

:- use_module(kahinasicstus).

:- dynamic kbreakpoint/1.

ktrace :-
  noktrace,
  disable_breakpoints(all),
  add_breakpoint([\+module(ktrace),\+pred(ktrace/0),\+pred(noktrace/0),\+pred(nodebug/0),\+pred(halt/0),(call;fail;exit;redo;exception;block;unblock)]-[kahina_breakpoint_action],Breakpoint),
  nodebug,
  assert(kbreakpoint(Breakpoint)),
  debug.

noktrace :-
  nodebug,
  retract(kbreakpoint(Breakpoint)),
  remove_breakpoints([Breakpoint]),
  !.
noktrace.

:- set_prolog_flag(source_info,on).
