:- module(kahinasicstus,[]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(jasper)).
:- use_module(library(system)).

:- multifile user:breakpoint_expansion/2.

user:breakpoint_expansion(kahina_breakpoint_action,[
    silent,
    inv(Inv),
    true(kahinasicstus:kahina_breakpoint_action(Inv,Action)),
    (true(Action == s)
    -> skip(Inv)
     ; (true(Action == f)
       -> \+ port(fail),
          fail(Inv)
        ; proceed))]).

kahina_breakpoint_action(Inv,Action) :-
  execution_state(port(Port)),
  get_bridge(Bridge),
  act(Port,Inv,Bridge),
  (execution_state(line(File,Line))	% The source_info flag has to be set to on or to emacs and not all goals have line information associated with them.
  -> write_to_chars(File,FileChars),
     register_source_code_location(Bridge,Inv,FileChars,Line)
   ; true),
  get_action(Bridge,Action).

act(call,Inv,Bridge) :-
  execution_state(pred(Module:Pred)),	% "module qualified goal template", see manual
  write_to_chars(Module:Pred,PredChars),
  act_step(Bridge,Inv,PredChars),
  act_call(Bridge,Inv).
act(fail,Inv,Bridge) :-
  act_fail(Bridge,Inv).
act(exit(nondet),Inv,Bridge) :-
  !,
  act_exit(Bridge,Inv,false).
act(exit(det),Inv,Bridge) :-
  act_exit(Bridge,Inv,true).
act(redo,Inv,Bridge) :-
  act_redo(Bridge,Inv).

act_step(Bridge,Inv,PredChars) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','step',[instance]),
      step(+object('org/kahina/prolog/bridge/PrologBridge'),+integer,+chars),
      step(Bridge,Inv,PredChars)).

act_call(Bridge,Inv) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','call',[instance]),
      call(+object('org/kahina/prolog/bridge/PrologBridge'),+integer),
      call(Bridge,Inv)).

act_fail(Bridge,Inv) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','fail',[instance]),
      fail(+object('org/kahina/prolog/bridge/PrologBridge'),+integer),
      fail(Bridge,Inv)).

act_exit(Bridge,Inv,Det) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','exit',[instance]),
      exit(+object('org/kahina/prolog/bridge/PrologBridge'),+integer,+boolean),
      exit(Bridge,Inv,Det)).

act_redo(Bridge,Inv) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','redo',[instance]),
      redo(+object('org/kahina/prolog/bridge/PrologBridge'),+integer),
      redo(Bridge,Inv)).

register_source_code_location(Bridge,Inv,FileChars,Line) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','registerStepSourceCodeLocation',[instance]),
      register_step_source_code_location(+object('org/kahina/prolog/bridge/PrologBridge'),+integer,+chars,+integer),
      register_step_source_code_location(Bridge,Inv,FileChars,Line)).

% ------------------------------------------------------------------------------
% CONTROL
% ------------------------------------------------------------------------------

get_action(Bridge,Action) :-
  wait_for_action(Bridge,Action),
  !.

wait_for_action(Bridge,Action) :-
  repeat,
  get_action_from_bridge(Bridge,Action),
  (Action == 110 % n
  -> (sleep(0.1),
      fail)
   ; true).

get_action_from_bridge(Bridge,Action) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','getAction',[instance]),
      get_action(+object('org/kahina/prolog/bridge/PrologBridge'),[-char]),
      get_action(Bridge,Action)).

% ------------------------------------------------------------------------------
% INSTANCE/SESSION/BRIDGE MANAGEMENT
% ------------------------------------------------------------------------------

get_bridge(Bridge) :-
  memory(kahina_bridge(Bridge)),
  ensure_bridge_exists(Bridge).

ensure_bridge_exists(Bridge) :-
  var(Bridge),
  !,
  start_new_kahina_session(Bridge).
ensure_bridge_exists(_).

start_new_kahina_session(Bridge) :-
  get_kahina_instance(Instance),
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/PrologDebuggerInstance','startNewSession',[instance]),
      start_new_session(+object('org/kahina/prolog/PrologDebuggerInstance'),[-object('org/kahina/prolog/bridge/PrologBridge')]),
      start_new_session(Instance,Bridge)),
  write_to_chars('[query]',RootLabelChars),
  act_step(Bridge,0,RootLabelChars),
  act_call(Bridge,0).

:- dynamic kahina_instance/1.

get_kahina_instance(Instance) :-
  kahina_instance(Instance),
  !.
get_kahina_instance(Instance) :-
  get_jvm(JVM),
  jasper_new_object(JVM,'org/kahina/prolog/PrologDebuggerInstance',init,init,LocalInstance),
  jasper_create_global_ref(JVM,LocalInstance,Instance),
  assert(kahina_instance(Instance)).

:- dynamic jvm/1.

get_jvm(JVM) :-
  jvm(JVM),
  !.
get_jvm(JVM) :-
  jasper_initialize(JVM), % TODO classpath, heap size?
  assert(jvm(JVM)).

% ------------------------------------------------------------------------------
% UTILITIES
% ------------------------------------------------------------------------------

memory(Term) :-
  execution_state(private(P)),
  memberchk(Term,P).
