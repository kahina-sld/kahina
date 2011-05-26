:- module(qtrace,[qtrace/0,
                  noqtrace/0]).

:- use_module('../kahinasicstus/kahinasicstus').
:- use_module(library(system)).

:- dynamic qbreakpoint/2. % qbreakpoint(Module:Functor/Arity,BID)

% First approximation to a QType-specific tracer:
% Sets a breakpoint with kahina_breakpoint_action on every QType predicate with
% at least one clause that has an immediate msg/3 subgoal.
qtrace :-
  qtype_home(Home),
  set_breakpoints(Home).

noqtrace :-
  retract(qbreakpoint(_,BID)),
  remove_breakpoints([BID]),
  fail.
noqtrace.

% failure-driven
set_breakpoints(Home) :-
  directory_files(Home,Files),
  atom_codes('.pl',ExtensionCodes),
  member(File,Files),
  atom_codes(File,FileCodes),
  ends_with(FileCodes,ExtensionCodes),
  open(File,read,Stream,[eof_action(eof_code)]),
  set_breakpoints_file(Stream,_),
  close(Stream),
  fail.
set_breakpoints(_).

% recursive, fails
set_breakpoints_file(Stream,ModuleHint) :-
  read(Stream,Term),
  set_breakpoints_term(Term,ModuleHint),
  set_breakpoints_file(Stream,ModuleHint).

% fails on end of file
set_breakpoints_term((:- module(ModuleHint)),ModuleHint) :-
  !.
set_breakpoints_term((:- module(ModuleHint,_)),ModuleHint) :-
  !.
set_breakpoints_term(end_of_file,_) :-
  !,
  fail.
set_breakpoints_term((Head :- Body),ModuleHint) :-
  !,
  hint_module(ModuleHint,Module),
  set_breakpoints_clause(Module,Head,Body).  
set_breakpoints_term(_,_).

% always succeeds
set_breakpoints_clause(Module,Head,Body) :-
  module_head_pred(Module,Head,Pred),
  \+ qbreakpoint(Pred,_),
  has_subgoal(Body,msg(_,_,_)),
  add_breakpoint([pred(Pred),(call;fail;exit;redo;exception;block;unblock)]-[kahinasicstus_breakpoint_action],BID),
  assert(Pred,BID).
set_breakpoints_clause(_,_,_).

module_head_pred(_,Module:Head,Module:Functor/Arity) :-
  !,
  functor(Head,Functor,Arity).
module_head_pred(Module,Head,Module:Functor/Arity) :-
  functor(Head,Functor,Arity).

has_subgoal((Subgoal,_),Target) :-
  subsumes(Target,Subgoal), % can't just unify because variables could appear as subgoals
  !.
has_subgoal((_,Rest),Target) :-
  has_subgoal(Rest,Target).

qtype_home(Home) :-
  environ('QTYPE_HOME',Home),
  !.
qtype_home(_) :-
  raise_exception(qtype_home_not_set).

user:generate_message_hook(qtype_home_not_set,[format('ERROR: Environment variable QTYPE_HOME must be set to path of directory',[]),nl,format('containing qtype.pl',[]),nl|Tail],Tail).

ends_with(List,List) :-
  !.
ends_with([_|Rest],Suffix) :-
  ends_with(Rest,Suffix).

hint_module(Hint,user) :-
  var(Hint),
  !.
hint_module(Module,Module).
