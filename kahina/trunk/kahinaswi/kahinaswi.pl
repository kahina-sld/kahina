:- module(kahinaswi,[]).

:- use_module(actions,[map_action/3,fix_action/3]).
:- use_module(library(jpl)).

:- dynamic bridge/1.

start(Bridge) :-
  jpl_call(class([org,kahina,prolog],['PrologDebuggerRunner']),runAndGetBridge,
      [],Bridge),
  assert(bridge(Bridge)).

get_bridge(Bridge) :-
  bridge(Bridge),
  !.
get_bridge(Bridge) :-
  start(Bridge).

user:prolog_trace_interception(Port,Frame,_Choice,Action) :-
  % TODO check if is Kahina-SWI is switched on, there should be some flag.
  get_bridge(Bridge),
  !,
  act(Port,Frame,Bridge),
  get_gui_action(Bridge,GUIAction),
  fix_action(Port,GUIAction,FixedGUIAction),
  map_action(FixedGUIAction,_,Action).

act(call,Frame,Bridge) :-
  % TODO use own, nicer IDs
  get_parent_id(Frame,ParentID),
  prolog_frame_attribute(Frame,goal,Goal),
  term_to_atom(Goal,GoalAtom), % TODO shorten, extra view for full goals
  jpl_call(Bridge,step,[Frame,GoalAtom],_),
  jpl_call(Bridge,call,[Frame,ParentID],_).
act(fail,Frame,Bridge) :-
  jpl_call(Bridge,fail,[Frame],_).
act(exit,Frame,Bridge) :-
  prolog_frame_attribute(Frame,has_alternatives,HasAlternatives),
  not(HasAlternatives,Deterministic),
  jpl_call(Bridge,exit,[Frame,@(Deterministic)],_).
act(redo,Frame,Bridge) :-
  jpl_call(Bridge,redo,[Frame]).
% TODO exception handling

get_parent_id(Frame,ParentID) :-
  prolog_frame_attribute(Frame,parent,ParentID),
  !.
get_parent_id(_,-1).

get_gui_action(Bridge,GUIAction) :-
  repeat,
  jpl_call(Bridge,getAction,[],Char),
  (char_command(Char,GUIAction)
  -> !
   ; (sleep(0.1),
      fail)).

not(true,false).
not(false,true).

char_command(115,skip). % s
char_command(99,creep). % c
char_command(102,fail). % f
char_command(97,abort). % a
