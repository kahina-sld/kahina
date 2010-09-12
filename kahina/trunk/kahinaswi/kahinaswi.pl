:- module(kahinaswi,[]).

:- use_module(library(jpl)).

:- dynamic bridge/1.

start :-
  jpl_call(class([org,kahina,prolog],['PrologDebuggerRunner']),runAndGetBridge,
      [],Bridge),
  assert(bridge(Bridge)).

user:prolog_trace_interception(Port,Frame,_Choice,continue) :-
  bridge(Bridge),
  !,
  act(Port,Frame,Bridge).
user:prolog_trace_interception(_,_,_,_).

act(call,Frame,Bridge) :-
  prolog_frame_attribute(Frame,parent,ParentFrame),
  prolog_frame_attribute(Frame,goal,Goal),
  term_to_atom(Goal,GoalAtom), % TODO shorten, extra view for full goals
  jpl_call(Bridge,step,[Frame,GoalAtom],_),
  jpl_call(Bridge,call,[Frame,ParentFrame],_).
act(fail,Frame,Bridge) :-
  jpl_call(Bridge,fail,[Frame],_).
act(exit,Frame,Bridge) :-
  prolog_frame_attribute(Frame,has_alternatives,HasAlternatives),
  not(HasAlternatives,Deterministic),
  jpl_call(Bridge,exit,[Frame,@(Deterministic)],_).
act(redo,Frame,Bridge) :-
  jpl_call(Bridge,redo,[Frame]).
% TODO exception handling

% TODO receive commands from GUI

not(true,false).
not(false,true).
