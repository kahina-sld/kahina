:- module(qtrace,[qtrace/0,
                  noqtrace/0]).

:- use_module('../kahinasicstus/kahinasicstus').
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(terms)).

% ------------------------------------------------------------------------------
% STARTUP TEST
% ------------------------------------------------------------------------------

:- environ('QTYPE_HOME',_)
   -> ( use_module('$QTYPE_HOME/atts'),
        use_module('$QTYPE_HOME/ops') )
    ; raise_exception(qtype_home_not_set).


% ------------------------------------------------------------------------------
% HOOK IMPLEMENTATIONS
% ------------------------------------------------------------------------------

user:generate_message_hook(qtype_home_not_set,[format('ERROR: Environment variable QTYPE_HOME must be set to path of directory',[]),nl,format('containing qtype.pl',[]),nl|Tail],Tail).

:- multifile kahinasicstus:abort_hook/2.

% TODO The following should check if we really are in a qtrace session, or maybe
% there is a better way to customize the tracer behavior than hooks. Currently,
% loading this module renders the regular SICStus debugger unusable (which is
% not that much of an issue, but still...)

% We want to return to QType prompt, not SICStus prompt:
kahinasicstus:abort_hook(trace,exception(kahinaqtype_abort)).

:- multifile kahinasicstus:breakpoint_action_hook/4.

% This exception has a special meaning and should not be traced:
kahinasicstus:breakpoint_action_hook(exception(kahinaqtype_abort),_,debug,proceed).

:- dynamic qbreakpoint/2. % qbreakpoint(Module:Functor/Arity,BID)
% ------------------------------------------------------------------------------
% MAIN CODE
% ------------------------------------------------------------------------------

% First approximation to a QType-specific tracer:
% Sets a breakpoint with kahina_breakpoint_action on every QType predicate with
% at least one clause that has an immediate msg/3 subgoal.
qtrace :-
  qtype_home(Home),
  set_breakpoints(Home),
  debug.

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
  \+ not_parsed(File),
  atom_codes(File,FileCodes),
  ends_with(FileCodes,ExtensionCodes),
  absolute_file_name(File,AbsFile,[relative_to(Home)]),
  open(AbsFile,read,Stream,[eof_action(eof_code)]),
  set_breakpoints_file(Stream,_),
  close(Stream),
  fail.
set_breakpoints(_).

not_parsed('atts.pl').
not_parsed('ops.pl').

% recursive, always succeeds deterministically
set_breakpoints_file(Stream,ModuleHint) :-
  read(Stream,Term),
  set_breakpoints_term(Term,ModuleHint),
  !,
  set_breakpoints_file(Stream,ModuleHint).
set_breakpoints_file(_,_).

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
  \+ not_traced(Pred),
  \+ qbreakpoint(Pred,_),
  has_subgoal(Body,msg(_,_,_)),
  add_breakpoint([pred(Pred),(call;fail;exit;redo;exception;block;unblock)]-[kahina_breakpoint_action],BID),
  assert(qbreakpoint(Pred,BID)).
set_breakpoints_clause(_,_,_).

not_traced(timer:msg_timer/_).
not_traced(timer:msg_timer2/_).

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

ends_with(List,List) :-
  !.
ends_with([_|Rest],Suffix) :-
  ends_with(Rest,Suffix).

hint_module(Hint,user) :-
  var(Hint),
  !.
hint_module(Module,Module).

:- set_prolog_flag(source_info,on).
