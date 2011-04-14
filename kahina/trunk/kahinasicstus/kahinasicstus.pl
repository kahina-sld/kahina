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
    (true(Action == 115) % s(kip)
    -> skip(Inv),
       proceed
     ; (true(Action == 102) % f(ail)
       -> \+ port(fail),
          fail(Inv)
        ; true(Action == 97) % a(bort)
          -> abort
           ; proceed))]).

kahina_breakpoint_action(Inv,Action) :-
  execution_state(port(Port)),
  get_bridge(Bridge),
  act(Port,Inv,Bridge),
  (execution_state(line(File,Line))	% The source_info flag has to be set to on or to emacs and not all goals have line information associated with them.
  -> write_to_chars(File,FileChars),
     register_source_code_location(Bridge,Inv,FileChars,Line) % TODO port-specific?
   ; true),
  get_action(Port,Bridge,Action).

:- dynamic waiting_for_unblocked_goal/0.

act(call,Inv,Bridge) :-
  retract(unblock_pseudostep_waiting_for_link(UnblockingID)),
  execution_state(goal(Module:Goal)),
  recall_blocked_goal(Module:Goal,BlockingID),
  !,
  link_nodes(Bridge,UnblockingID,BlockingID),
  act(call,Inv,Bridge). % Continue with second clause. Can't fail because recall_blocked_goal/2 is supposed to change the execution state.
act(call,Inv,Bridge) :-
  execution_state(pred(Module:Pred)),	% "module qualified goal template", see manual
  write_to_chars(Module:Pred,PredChars),
  act_step(Bridge,Inv,PredChars),
  act_call(Bridge,Inv).
act(fail,Inv,Bridge) :-
  retractall(unblock_pseudostep_waiting_for_link(_)),
  act_fail(Bridge,Inv).
act(exit(nondet),Inv,Bridge) :-
  !,
  retractall(unblock_pseudostep_waiting_for_link(_)),
  act_exit(Bridge,Inv,false).
act(exit(det),Inv,Bridge) :-
    retractall(unblock_pseudostep_waiting_for_link(_)),
  act_exit(Bridge,Inv,true).
act(redo,Inv,Bridge) :-
    retractall(unblock_pseudostep_waiting_for_link(_)),
  act_redo(Bridge,Inv).
act(block,_,Bridge) :-
  retractall(unblock_pseudostep_waiting_for_link(_)), % TODO What if the unblocked step is immediately blocked, e.g. in freeze(X,freeze(Y,...))? freeze/2 isn't called, so we would have to do the linking here.
  execution_state(goal(Module:Goal)),
  remember_blocked_goal(Module:Goal,ID),
  execution_state(pred(Module:Pred)),
  write_to_chars(Module:Pred,PredChars),
  act_step(Bridge,ID,[98,108,111,99,107,32|PredChars]), % 'block '
  act_call(Bridge,ID),
  act_exit(Bridge,ID,true).
act(unblock,_,Bridge) :-
  retractall(unblock_pseudostep_waiting_for_link(_)),
  get_next_pseudostep_id(ID),
  execution_state(pred(Module:Pred)),
  write_to_chars(Module:Pred,PredChars),
  act_step(Bridge,ID,[117,110,98,108,111,99,107,32|PredChars]), % 'unblock '
  act_call(Bridge,ID),
  act_exit(Bridge,ID,true),
  % Would be nicer to have the unblocked steps as children of the unblock step
  % rather than siblings, but we don't know how many there are.
  assert(unblock_pseudostep_waiting_for_link(ID)).
% TODO handle exception ports

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
      fail(Bridge,Inv)),
  end(Inv,Bridge).

act_exit(Bridge,Inv,Det) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','exit',[instance]),
      exit(+object('org/kahina/prolog/bridge/PrologBridge'),+integer,+boolean),
      exit(Bridge,Inv,Det)),
  end(Inv,Bridge).

act_redo(Bridge,Inv) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','redo',[instance]),
      redo(+object('org/kahina/prolog/bridge/PrologBridge'),+integer),
      redo(Bridge,Inv)).

end(1,Bridge) :-
  !,
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','end',[instance]),
      end(+object('org/kahina/prolog/bridge/PrologBridge'),+integer),
      end(Bridge,1)).
end(_,_).

register_source_code_location(Bridge,Inv,FileChars,Line) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','registerStepSourceCodeLocation',[instance]),
      register_step_source_code_location(+object('org/kahina/prolog/bridge/PrologBridge'),+integer,+chars,+integer),
      register_step_source_code_location(Bridge,Inv,FileChars,Line)).

link_nodes(Bridge,Anchor,Target) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/prolog/bridge/PrologBridge','linkNodes',[instance]),
      link_nodes(+object('org/kahina/prolog/bridge/PrologBridge'),+integer,+integer),
      link_nodes(Bridge,Anchor,Target)).

% ------------------------------------------------------------------------------
% CONTROL
% ------------------------------------------------------------------------------

get_action(unblock,_,99) :- % c
  !.
get_action(_,Bridge,Action) :-
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

% Since we use Prolog's invocation numbers for step IDs, we need a
% non-conflicting number space for pseudostep IDs... like negative numbers.

:- dynamic next_pseudostep_id/1.

initialize_pseudostep_id :-
  retractall(next_pseudostep_id(_)),
  assert(next_pseudostep_id(-2)). % start here because -1 is sometimes used as a null equivalent

get_next_pseudostep_id(ID) :-
  retract(next_pseudostep_id(ID)),
  NewID is ID - 1,
  assert(next_pseudostep_id(NewID)).

start_new_kahina_session(Bridge) :-
  initialize_pseudostep_id,
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
% BLOCKED GOALS
% Here, unblock steps are matched to corresponding block steps in the history.
% Some guesswork is involved.
% ------------------------------------------------------------------------------

% analyze_blocked_goal(+Goal,-Condition)
% For a given blocked goal, gives in the form of a goal a necessary (but alas,
% not sufficient) condition for it to be unblocked.
analyze_blocked_goal(_:when(Condition,Goal),Condition,Goal) :-
  !.
analyze_blocked_goal(_:freeze(Var,Goal),nonvar(Var),Goal) :-
  !.
analyze_blocked_goal(_:dif(X,Y),?=(X,Y),0) :- % no goal - what TODO about this?
  !.
analyze_blocked_goal(Module:Goal,true,Module:Goal).
% The last clause is for goals blocked by block declarations. Here we don't know
% the exact condition since we don't know which BlockSpec caused the blocking.
% We could of course apply a tighter heuristic and require that there is at
% least one BlockSpec that evaluates to true at block time such that one of the
% arguments it marks by - is bound at unblock time. But I guess this would in
% practice lead to only very few additional correct guesses.

remember_blocked_goal(Module:Goal,ID) :-
  analyze_blocked_goal(Module:Goal,Condition,TargetModule:TargetGoal), % could fail if we used the tighter heuristic
  get_next_pseudostep_id(ID),
  memory(blocked_goals(BlockedGoals)),
  (reduce_goal(TargetGoal,TargetModule,ReducedTarget)
  -> memberchk(blocked_goal(ID,ReducedTarget,Condition,_),BlockedGoals) % last arg will be instantiated later to mark goal as unblocked
   ; true). % We will not be able to trace any unblock step back to this block step.

reduce_goal(Goal,Module,Reduced) :-
goal_behavior(Goal,Module,Behavior),
  goal_behavior(Goal,Module,call(Reduced)).

% goal_behavior(+Goal,+Module,-Behavior)
% Predicts the behavior of a given goal when called in trace mode, taking into
% account the fact that the control predicates !/0, true/0, fail/0, ,/2, ->/2,
% ;/2, if/3 and once/1 do not show up in the trace. Possible resulting values
% for Behavior are:
%   true - the goal will succeed without trace
%   fail - the goal will fail without trace
%   call(G) - the tracer will show the call port of a procedure box with goal G
% Clauses are grouped by the form of Goal.
% Module:Goal
goal_behavior(Module:Goal,_,Behavior) :-
  !,
  goal_behavior(Goal,Module,Behavior).
% !
goal_behavior(!,_,true) :- % TODO take special effect of cut into account - e.g. in ((!,fail);a) the behavior is fail but is predicted as a, right?
  !.
% true
goal_behavior(true,_,true) :-
  !.
% fail
goal_behavior(fail,_,fail) :-
  !.
% A,B
goal_behavior((A,_),Module,fail) :-
  goal_behavior(A,Module,fail),
  !.
goal_behavior((A,B),Module,Behavior) :-
  goal_behavior(A,Module,true),
  !,
  goal_behavior(B,Module,Behavior).
goal_behavior((A,_),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% A -> B
goal_behavior((A -> _),Module,fail) :-
  goal_behavior(A,Module,fail),
  !.
goal_behavior((A -> B),Module,Behavior) :-
  goal_behavior(A,Module,true),
  !,
  goal_behavior(B,Module,Behavior).
goal_behavior((A -> _),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% A;B
goal_behavior((A;B),Module,Behavior) :-
  goal_behavior(A,Module,fail),
  !,
  goal_behavior(B,Module,Behavior).
goal_behavior((A;_),Module,true) :-
  goal_behavior(A,Module,true),
  !.
goal_behavior((A;_),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% \+A
goal_behavior(\+A,Module,true) :-
  !,
  goal_behavior(A,Module,fail).
goal_behavior(\+A,Module,fail) :-
  !,
  goal_behavior(A,Module,true).
goal_behavior(\+A,Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% if(A,B,C)
goal_behavior(if(A,_,C),Module,Behavior) :-
  goal_behavior(A,Module,fail),
  !,
  goal_behavior(C,Module,Behavior).
goal_behavior(if(A,B,_),Module,Behavior) :-
  goal_behavior(A,Module,true),
  !,
  goal_behavior(B,Module,Behavior).
goal_behavior(if(A,_,_),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% once(A)
goal_behavior(once(A),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% other forms
goal_behavior(Goal,Module,call(Module:Goal)).

% recall_blocked_goal(+Goal,-ID)
% To be called when Goal is unblocked. Succeeds if it was previously remembered
% blocked, and instantiates ID to the ID of the corresponding block pseudostep.
recall_blocked_goal(Module:Goal,ID) :-
  memory(blocked_goals(BlockedGoals)),
  member(blocked_goal(ID,Module2:Goal2,Condition,Unblocked),BlockedGoals),
  (var(ID) % end of open-ended list reached
  -> !, fail
   ; true),
  var(Unblocked), % not unblocked yet in the current stack
  % Check if it's the same goal. If two literally identical goals are blocked,
  % and unblocked in reverse order, our guess at the ID will be wrong. Checking
  % the unblock condition might prevent this but cannot fully do so because it
  % is not always sufficient, and does not help when both conditions become true
  % simultaneously. In the latter case, we can hope that goals are unblocked in
  % the order they were blocked, but SP does not guarantee this.
  Module == Module2,
  Goal == Goal2,
  Condition,
  Unblocked = true,
  !.

% ------------------------------------------------------------------------------
% UTILITIES
% ------------------------------------------------------------------------------

memory(Term) :-
  execution_state(private(P)),
  memberchk(Term,P).
