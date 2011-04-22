:- module(kahinasicstus,[]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(jasper)).
:- use_module(library(system)).
:- use_module(library(terms)).

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
  get_action(Port,Bridge,Action).

:- dynamic unblocked_pseudostep_waiting_for_link/1.

act(call,Inv,Bridge) :-
  retract(unblock_pseudostep_waiting_for_link(UnblockingID)),
  execution_state(goal(Module:Goal)),
  recall_blocked_goal(Module:Goal,BlockingID),
  !,
  link_nodes(Bridge,UnblockingID,BlockingID),
  act(call,Inv,Bridge). % Continue with second clause. Can't just fail because recall_blocked_goal/2 is supposed to change the execution state.
act(call,Inv,Bridge) :-
  execution_state(pred(Module:Pred)),	% "module qualified goal template", see manual
  write_to_chars(Module:Pred,PredChars),
  act_step(Bridge,Inv,PredChars),
  (execution_state(line(File,Line))	% The source_info flag has to be set to on or to emacs and not all goals have line information associated with them.
  -> write_to_chars(File,FileChars),
     register_source_code_location(Bridge,Inv,FileChars,Line)
   ; true),
  act_call(Bridge,Inv),
  perhaps(send_variable_bindings(Bridge,Inv,call)).
act(fail,Inv,Bridge) :-
  retractall(unblock_pseudostep_waiting_for_link(_)),
  act_fail(Bridge,Inv).
act(exit(nondet),Inv,Bridge) :-
  !,
  retractall(unblock_pseudostep_waiting_for_link(_)),
  act_exit(Bridge,Inv,false),
  perhaps(send_variable_bindings(Bridge,Inv,exit(nondet))).
act(exit(det),Inv,Bridge) :-
    retractall(unblock_pseudostep_waiting_for_link(_)),
  act_exit(Bridge,Inv,true),
  perhaps(send_variable_bindings(Bridge,Inv,exit(det))).
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
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','step',[instance]),
      step(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+chars),
      step(Bridge,Inv,PredChars)).

act_call(Bridge,Inv) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','call',[instance]),
      call(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer),
      call(Bridge,Inv)).

act_fail(Bridge,Inv) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','fail',[instance]),
      fail(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer),
      fail(Bridge,Inv)),
  end(Inv,Bridge).

act_exit(Bridge,Inv,Det) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','exit',[instance]),
      exit(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+boolean),
      exit(Bridge,Inv,Det)),
  end(Inv,Bridge).

act_redo(Bridge,Inv) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','redo',[instance]),
      redo(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer),
      redo(Bridge,Inv)).

end(1,Bridge) :-
  !,
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','end',[instance]),
      end(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer),
      end(Bridge,1)).
end(_,_).

register_source_code_location(Bridge,Inv,FileChars,Line) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','registerStepSourceCodeLocation',[instance]),
      register_step_source_code_location(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+chars,+integer),
      register_step_source_code_location(Bridge,Inv,FileChars,Line)).

link_nodes(Bridge,Anchor,Target) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','linkNodes',[instance]),
      link_nodes(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+integer),
      link_nodes(Bridge,Anchor,Target)).

send_variable_bindings(Bridge,Inv,Port) :-
  get_jvm(JVM),
  variable_binding(Name,Value), % has many solutions
  %send_variable_binding(JVM,Bridge,Inv,Port,Name,Value),
  fail.
send_variable_binding(_,_,_).

send_variable_binding(JVM,Bridge,Inv,Port,Name,Value) :-
  port_direction(Port,DirectionChars),
  write_to_chars(Name,NameChars),
  write_to_chars(Value,ValueChars),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','registerBinding',[instance]),
      register_bindings(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+chars,+chars,+chars),
      register_bindings(Bridge,Inv,DirectionChars,NameChars,ValueChars)).

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
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','getAction',[instance]),
      get_action(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),[-char]),
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
  retractall(source_read(_)),
  retractall(source_clause(_,_,_,_)),
  get_kahina_instance(Instance),
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/SICStusPrologDebuggerInstance','startNewSession',[instance]),
      start_new_session(+object('org/kahina/sicstus/SICStusPrologDebuggerInstance'),[-object('org/kahina/sicstus/bridge/SICStusPrologBridge')]),
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
  jasper_new_object(JVM,'org/kahina/sicstus/SICStusPrologDebuggerInstance',init,init,LocalInstance),
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
% SOURCE INFORMATION
% Mainly variable bindings.
% ------------------------------------------------------------------------------

:- dynamic source_read/1.
:- dynamic source_clause/5.

variable_binding(Name,Value) :-
  execution_state(line(File,Line)), % TODO we do that twice at call ports, consolidate
  (source_read(File) % TODO is that guaranteed to be absolute?
  -> true
   ; read_source_file(File)),
  execution_state(parent_clause(Clause)),
  once((source_clause(SourceClause,File,FirstLine,LastLine,Names),subsumes(SourceClause,Clause),FirstLine=<Line,LastLine>=Line)), % TODO with really bad code organization, we might retrieve a wrong clause here
  member(Name=Value,Names).

read_source_file(AbsFileName) :-
  open(AbsFileName,read,Stream,[eof_action(eof_code)]),
  repeat,
    read_term(Stream,Term,[variable_names(Names),layout(Layout)]),
    handle_term(Term,Names,Layout,AbsFileName),
  !,
  close(Stream),
  assert(source_read(AbsFileName)).

handle_term(end_of_file,_,_,_) :-
  !.
handle_term(Clause,Names,Layout,AbsFileName) :-
  first_line(Layout,FirstLine),
  last_line(Layout,LastLine),
  assert(source_clause(Clause,AbsFileName,FirstLine,LastLine,Names)),
  fail.

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

reduce_goal(Goal,Module,Reduced) :-
  goal_behavior(Goal,Module,call(Reduced)).

% goal_behavior(+Goal,+Module,-Behavior)
% Predicts the behavior of a given goal when called in trace mode, taking into
% account the fact that the control predicates !/0, true/0, fail/0, ,/2, ->/2,
% ;/2, if/3 and once/1 do not show up in the trace. Possible resulting values
% for Behavior are:
%   true(cut) - the goal will succeed without trace and contains a cut
%   true(nocut) - the goal will succed without trace and contains no cut
%   fail(cut) - the goal will fail without trace and contains a cut
%   fail(nocut) - the goal will fail without trace and contains no cut
%   call(G) - the tracer will show the call port of a procedure box with goal G
% Clauses are grouped by the form of Goal.
% Module:Goal
goal_behavior(Module:Goal,_,Behavior) :-
  !,
  goal_behavior(Goal,Module,Behavior).
% !
goal_behavior(!,_,true(cut)) :-
  !.
% true
goal_behavior(true,_,true(nocut)) :-
  !.
% fail
goal_behavior(fail,_,fail(nocut)) :-
  !.
% A,B
goal_behavior((A,_),Module,fail(Cut)) :-
  goal_behavior(A,Module,fail(Cut)),
  !.
goal_behavior((A,B),Module,Behavior) :-
  goal_behavior(A,Module,true(Cut)),
  !,
  goal_behavior(B,Module,Behavior0),
  add_cut(Behavior0,Cut,Behavior).
goal_behavior((A,_),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% A -> B
goal_behavior((A->_),Module,fail(nocut)) :-
  goal_behavior(A,Module,fail(_)), % scope of cut limited to A
  !.
goal_behavior((A->B),Module,Behavior) :-
  goal_behavior(A,Module,true(_)), % scope of cut limited to A
  !,
  goal_behavior(B,Module,Behavior).
goal_behavior((A->_),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% A;B
goal_behavior((A;_),Module,fail(cut)) :-
  goal_behavior(A,Module,fail(cut)),
  !.
goal_behavior((A;B),Module,Behavior) :-
  goal_behavior(A,Module,fail(nocut)),
  !,
  goal_behavior(B,Module,Behavior).
goal_behavior((A;_),Module,true(Cut)) :-
  goal_behavior(A,Module,true(Cut)),
  !.
goal_behavior((A;_),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% \+A
goal_behavior(\+A,Module,true(nocut)) :-
  !,
  goal_behavior(A,Module,fail(_)). % scope of cut limited to A
goal_behavior(\+A,Module,fail(nocut)) :-
  !,
  goal_behavior(A,Module,true(_)). % scope of cut limited to A
goal_behavior(\+A,Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% if(A,B,C)
goal_behavior(if(A,_,C),Module,Behavior) :-
  goal_behavior(A,Module,fail(_)), % scope of cut limited to A
  !,
  goal_behavior(C,Module,Behavior).
goal_behavior(if(A,B,_),Module,Behavior) :-
  goal_behavior(A,Module,true(_)), % scope of cut limited to A
  !,
  goal_behavior(B,Module,Behavior).
goal_behavior(if(A,_,_),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% once(A)
goal_behavior(once(A),Module,fail(nocut)) :-
  goal_behavior(A,Module,fail(_)), % scope of cut limited to A
  !.
goal_behavior(once(A),Module,true(nocut)) :-
  goal_behavior(A,Module,true(_)), % scope of cut limited to A
  !.
goal_behavior(once(A),Module,Behavior) :-
  !,
  goal_behavior(A,Module,Behavior).
% other forms
goal_behavior(Goal,Module,call(Module:Goal)).

add_cut(true(Cut1),Cut2,true(Cut)) :-
  cut_or(Cut1,Cut2,Cut).
add_cut(fail(Cut1),Cut2,fail(Cut)) :-
  cut_or(Cut1,Cut2,Cut).
add_cut(call(Goal),_,call(Goal)).

cut_or(nocut,nocut,nocut) :-
  !.
cut_or(_,_,cut).

% ------------------------------------------------------------------------------
% UTILITIES
% ------------------------------------------------------------------------------

memory(Term) :-
  execution_state(private(P)),
  memberchk(Term,P).

port_direction(call,[105,110]). % in
port_direction(fail,[111,117,116]). % out
port_direction(redo,[105,110]).
port_direction(exit(_),[111,117,116]).
port_direction(exception,[111,117,116]).

perhaps(Goal) :-
  Goal,
  !.
perhaps(_).

first_line([FirstLine|_],FirstLine) :-
  !.
first_line(FirstLine,FirstLine).

last_line(LastLine,LastLine) :-
  integer(LastLine),
  !.
last_line([LastArg],LastLine) :-
  !,
  last_line(LastArg,LastLine).
last_line([_|Args],LastLine) :-
  last_line(Args,LastLine).
