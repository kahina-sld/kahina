:- module(kahinaswi,[]).

:- use_module(actions,[map_action/3,fix_action/3]).
:- use_module(library(jpl)).

:- dynamic bridge/1.
:- dynamic frame_step/2.
:- dynamic next_step/1.

start(Bridge) :-
  jpl_call(class([org,kahina,prolog],['PrologDebuggerRunner']),runAndGetBridge,
      [],Bridge),
  assert(bridge(Bridge)),
  retractall(next_step(_)), % TODO more systematic cleanup
  assert(next_step(1)).

get_bridge(Bridge) :-
  bridge(Bridge),
  !.
get_bridge(Bridge) :-
  start(Bridge).

user:prolog_trace_interception(Port,Frame,_Choice,Action) :-
  % TODO check if is Kahina-SWI is switched on, there should be some flag.
  % TODO what's with notrace?
  notrace((get_bridge(Bridge),
  act(Port,Frame,Bridge),
  get_gui_action(Bridge,GUIAction),
  fix_action(Port,GUIAction,FixedGUIAction),
  map_action(FixedGUIAction,_,Action))).

act(call,Frame,Bridge) :-
  %log(call,Frame),
  get_next_step(Step),
  retractall(frame_step(Frame,_)),
  assert(frame_step(Frame,Step)),
  prolog_frame_attribute(Frame,goal,Goal),
  term_to_atom(Goal,GoalAtom), % TODO shorten, extra view for full goals
  jpl_call(Bridge,step,[Step,GoalAtom],_),
  jpl_call(Bridge,call,[Step],_).
act(fail,Frame,Bridge) :-
  %log(fail,Frame),
  retract(frame_step(Frame,Step)),
  jpl_call(Bridge,fail,[Step],_),
  end(Step,Bridge).
act(exit,Frame,Bridge) :-
  %log(exit,Frame),
  frame_step(Frame,Step),
  prolog_frame_attribute(Frame,has_alternatives,HasAlternatives),
  not(HasAlternatives,Deterministic),
  jpl_call(Bridge,exit,[Step,@(Deterministic)],_),
  end(Step,Bridge).
act(redo,Frame,Bridge) :-
  %log(redo,Frame),
  frame_step(Frame,Step),
  jpl_call(Bridge,redo,[Step],_).
% TODO exception handling

get_next_step(Step) :-
  retract(next_step(Step)),
  NewStep is Step + 1,
  assert(next_step(NewStep)).

get_gui_action(Bridge,GUIAction) :-
  repeat,
  jpl_call(Bridge,getAction,[],Char),
  (char_command(Char,GUIAction)
  -> !
   ; (sleep(0.1),
      fail)).

end(1,Bridge) :-
  !,
  jpl_call(Bridge,end,[1],_).
end(_,_).

% TODO how to present user with solution, option to request another solution, etc.?

not(true,false).
not(false,true).

char_command(115,skip). % s
char_command(99,creep). % c
char_command(102,fail). % f
char_command(97,abort). % a

log(Port,Frame) :-
  prolog_frame_attribute(Frame,pc,PC),
  write(log(Port,Frame,pc:PC)),
  nl.
