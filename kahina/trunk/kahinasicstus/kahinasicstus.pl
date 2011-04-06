:- module(kahinasicstus,[]).

:- use_module(library(lists)).
:- use_module(library(jasper)).

:- multifile user:breakpoint_expansion/2.

user:breakpoint_expansion(kahina_breakpoint_action,[
    silent,
    true(kahinasicstus:kahina_breakpoint_action(Action)),
    (true(Action == s)
    -> inv(Inv),
       skip(Inv)
     ; (true(Action == f)
       -> \+ port(fail),
          inv(Inv),
          fail(Inv)
        ; true))]).

kahina_breakpoint_action(Action) :-
  (execution_state(line(File,Line))	% The source_info flag has to be set to on or to emacs and not all goals have line information associated with them.
  -> true
   ; true),
  execution_state(goal(Module:Goal)),	% Fails if Module:Goal is replaced with a single variable. See manual, Section 7.9.1, "Tests Related to the Current Goal".
  execution_state(port(Port)),
  get_bridge(Bridge),
  % TODO Communicate with Kahina. Ports to consider: call, fail, redo, exit(det), exit(nondet), block, unblock; what about exception?
  write(File),nl,write(Line),nl,write(Module),nl,write(Goal),nl,write(Port),nl.

get_bridge(Bridge) :-
  es(kahina_bridge(Bridge)),
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
      start_new_session(Instance,Bridge)).

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
% Utility predicate for storing data in the debugger trace
% ------------------------------------------------------------------------------

es(Term) :-
  execution_state(private(P)),
  memberchk(Term,P).
