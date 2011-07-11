:- module(kahinasicstus,[end_trace_session/0]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(jasper)).
:- use_module(library(system)).
:- use_module(library(terms)).

end_trace_session :-
  retractall(bridge(_)).

:- multifile user:breakpoint_expansion/2.

user:breakpoint_expansion(kahina_breakpoint_action,[
    % The three action variables Show, Mode, Command control Prolog's behavior
    % on encountering a breakpoint. We use show/1, mode/1, command/1 terms to
    % set their values. We always set Show to silent (no output on console). The
    % values for Mode and Command depend on the response from the GUI. For lists
    % of possible values for each action variable, see section 7.9.9 of the SP3
    % manual.
    show(silent),
    inv(Inv),
    port(Port),
    true(kahinasicstus:kahina_breakpoint_action(Inv,Port,Mode,Command)),
    mode(Mode),
    command(Command)]).

kahina_breakpoint_action(Inv,Port,Mode,Command) :-
  get_bridge(Inv,Port,Bridge),
  get_jvm(JVM),
  act(Port,Inv,Bridge,JVM),
  get_action(Port,Bridge,JVM,Action),
  action_mode_command(Action,Mode,Command,Inv,Port).

action_mode_command(115,skip(Inv),proceed,Inv,_Port) :- % s(kip)
  !.
action_mode_command(102,trace,proceed,_Inv,fail) :-     % f(ail)
  !.
action_mode_command(102,trace,fail(Inv),Inv,_Port) :-
  !.
action_mode_command(97,trace,abort,_Inv,_Port) :-       % a(bort)
  !,
  end_trace_session. % TODO necessary?
action_mode_command(_,debug,proceed,_Inv,_Port).

:- dynamic unblocked_pseudostep_waiting_for_link/1.

act(call,Inv,Bridge,JVM) :-
  retract(unblock_pseudostep_waiting_for_link(UnblockingID)),
  execution_state(goal(Module:Goal)),
  recall_blocked_goal(Module:Goal,BlockingID),
  !,
  link_nodes(Bridge,JVM,UnblockingID,BlockingID),
  act(call,Inv,Bridge,JVM). % Continue with second clause. Can't just fail because recall_blocked_goal/2 is supposed to change the execution state.
act(call,Inv,Bridge,JVM) :-
  execution_state(pred(Module:Pred)),	% "module qualified goal template", see manual
  write_to_chars(Module:Pred,PredChars),
  execution_state(goal(_:Goal)),
  goal_desc(Module,Goal,GoalDesc),
  write_to_chars(GoalDesc,GoalDescChars),
  act_step(Bridge,JVM,Inv,PredChars,GoalDescChars),
  (execution_state(line(File,Line))	% The source_info flag has to be set to on or to emacs and not all goals have line information associated with them.
  -> write_to_chars(File,FileChars),
     register_source_code_location(Bridge,JVM,Inv,FileChars,Line)
   ; true),
  act_call(Bridge,JVM,Inv),
  perhaps(send_variable_bindings(Bridge,JVM,Inv,call)),write(what),nl.
act(fail,Inv,Bridge,JVM) :-
  retractall(unblock_pseudostep_waiting_for_link(_)),
  act_fail(Bridge,JVM,Inv).
act(exit(nondet),Inv,Bridge,JVM) :-
  !,
  retractall(unblock_pseudostep_waiting_for_link(_)),
  execution_state(pred(Module:_)),
  execution_state(goal(_:Goal)),
  goal_desc(Module,Goal,GoalDesc),
  write_to_chars(GoalDesc,GoalDescChars),
  act_exit(Bridge,JVM,Inv,false,GoalDescChars),
  perhaps(send_variable_bindings(Bridge,JVM,Inv,exit(nondet))).
act(exit(det),Inv,Bridge,JVM) :-
    retractall(unblock_pseudostep_waiting_for_link(_)),
  act_exit(Bridge,JVM,Inv,true),
  perhaps(send_variable_bindings(Bridge,JVM,Inv,exit(det))).
act(redo,Inv,Bridge,JVM) :-
    retractall(unblock_pseudostep_waiting_for_link(_)),
  act_redo(Bridge,JVM,Inv).
act(exception(Exception),Inv,Bridge,JVM) :-
  retractall(unblock_pseudostep_waiting_for_link(_)),
  write_to_chars(Exception,ExceptionChars),
  act_exception(Bridge,JVM,Inv,ExceptionChars).
act(block,_,Bridge,JVM) :-
  retractall(unblock_pseudostep_waiting_for_link(_)), % TODO What if the unblocked step is immediately blocked, e.g. in freeze(X,freeze(Y,...))? freeze/2 isn't called, so we would have to do the linking here.
  execution_state(goal(Module:Goal)),
  remember_blocked_goal(Module:Goal,ID),
  execution_state(pred(_:Pred)),
  write_to_chars(Module:Pred,PredChars),
  goal_desc(Module,Goal,GoalDesc),
  write_to_chars(GoalDesc,GoalDescChars),
  act_step(Bridge,JVM,ID,[98,108,111,99,107,32|PredChars],[98,108,111,99,107,32|GoalDescChars]), % 'block '
  act_call(Bridge,JVM,ID),
  act_exit(Bridge,JVM,ID,true).
act(unblock,_,Bridge,JVM) :-
  retractall(unblock_pseudostep_waiting_for_link(_)),
  get_next_pseudostep_id(ID),
  execution_state(pred(Module:Pred)),
  write_to_chars(Module:Pred,PredChars),
  execution_state(goal(_:Goal)),
  goal_desc(Module,Goal,GoalDesc),
  write_to_chars(GoalDesc,GoalDescChars),
  act_step(Bridge,JVM,ID,[117,110,98,108,111,99,107,32|PredChars],[117,110,98,108,111,99,107,32|GoalDescChars]), % 'unblock '
  act_call(Bridge,JVM,ID),
  act_exit(Bridge,JVM,ID,true),
  % Would be nicer to have the unblocked steps as children of the unblock step
  % rather than siblings, but we don't know how many there are.
  assert(unblock_pseudostep_waiting_for_link(ID)).

% TODO it might make sense to change the goal desc at the exit port (additional bindings!)
act_step(Bridge,JVM,Inv,PredChars,GoalDescChars) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','step',[instance]),
      step(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+chars,+chars),
      step(Bridge,Inv,PredChars,GoalDescChars)).

act_call(Bridge,JVM,Inv) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','call',[instance]),
      call(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer),
      call(Bridge,Inv)).

act_fail(Bridge,JVM,Inv) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','fail',[instance]),
      fail(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer),
      fail(Bridge,Inv)),
  end(Inv,Bridge,JVM).

act_exception(Bridge,JVM,Inv,ExceptionChars) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','exception',[instance]),
      exception(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+chars),
      exception(Bridge,Inv,ExceptionChars)),
  end(Inv,Bridge,JVM).

act_exit(Bridge,JVM,Inv,Det) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','exit',[instance]),
      exit(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+boolean),
      exit(Bridge,Inv,Det)),
  end(Inv,Bridge,JVM).

act_exit(Bridge,JVM,Inv,Det,GoalDescChars) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','exit',[instance]),
      exit(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+boolean,+chars),
      exit(Bridge,Inv,Det,GoalDescChars)),
  end(Inv,Bridge,JVM).

act_redo(Bridge,JVM,Inv) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','redo',[instance]),
      redo(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer),
      redo(Bridge,Inv)).

:- dynamic end_det/0.

end(1,Bridge,JVM) :-
  !,
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','select',[instance]),
      select(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer),
      select(Bridge,1)),
  assert(end_det).
end(_,_,_).

register_source_code_location(Bridge,JVM,Inv,FileChars,Line) :-
  get_jvm(JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','registerStepSourceCodeLocation',[instance]),
      register_step_source_code_location(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+chars,+integer),
      register_step_source_code_location(Bridge,Inv,FileChars,Line)).

link_nodes(Bridge,JVM,Anchor,Target) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','linkNodes',[instance]),
      link_nodes(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),+integer,+integer),
      link_nodes(Bridge,Anchor,Target)).

send_variable_bindings(Bridge,JVM,Inv,Port) :-
  variable_binding(Name,Value), % has many solutions
  send_variable_binding(Bridge,JVM,Inv,Port,Name,Value),
  fail.
send_variable_binding(_,_,_).

send_variable_binding(Bridge,JVM,Inv,Port,Name,Value) :-
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

get_action(unblock,_,_,99) :- % c
  !.
get_action(_,_,_,99) :-
  retract(end_det),
  !.
get_action(_,Bridge,JVM,Action) :-
  wait_for_action(Bridge,JVM,Action),
  !.

wait_for_action(Bridge,JVM,Action) :-
  repeat,
  get_action_from_bridge(Bridge,JVM,Action),
  (Action == 110 % n
  -> (sleep(0.1),
      fail)
   ; true).

get_action_from_bridge(Bridge,JVM,Action) :-
  jasper_call(JVM,
      method('org/kahina/sicstus/bridge/SICStusPrologBridge','getAction',[instance]),
      get_action(+object('org/kahina/sicstus/bridge/SICStusPrologBridge'),[-char]),
      get_action(Bridge,Action)).

% ------------------------------------------------------------------------------
% INSTANCE/SESSION/BRIDGE MANAGEMENT
% ------------------------------------------------------------------------------

:- dynamic bridge/1.

get_bridge(1,call,Bridge) :-
  !,
  get_jvm(JVM),
  start_new_kahina_session(Bridge,JVM),
  retractall(bridge(_)),
  assert(bridge(Bridge)).
get_bridge(_,_,Bridge) :-
  bridge(Bridge),
  !.
get_bridge(_,_,Bridge) :-
  get_jvm(JVM),
  start_new_kahina_session(Bridge,JVM),
  assert(bridge(Bridge)).

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

start_new_kahina_session(Bridge,JVM) :-
  initialize_pseudostep_id,
  retractall(source_read(_)),
  retractall(source_clause(_,_,_,_)),
  get_kahina_instance(Instance,JVM),
  jasper_call(JVM,
      method('org/kahina/sicstus/SICStusPrologDebuggerInstance','startNewSession',[instance]),
      start_new_session(+object('org/kahina/sicstus/SICStusPrologDebuggerInstance'),[-object('org/kahina/sicstus/bridge/SICStusPrologBridge')]),
      start_new_session(Instance,Bridge)),
  write_to_chars('[query]',RootLabelChars),
  act_step(Bridge,JVM,0,RootLabelChars,RootLabelChars),
  act_call(Bridge,JVM,0).

:- dynamic kahina_instance/1.

get_kahina_instance(Instance,_) :-
  kahina_instance(Instance),
  !.
get_kahina_instance(Instance,JVM) :-
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
:- dynamic source_clause/5. % TODO index by functor?

variable_binding(Name,Value) :-
  execution_state(line(File,Line)), % TODO we do that twice at call ports, consolidate
  (source_read(File) % TODO is that guaranteed to be absolute?
  -> true
   ; read_source_file(File)),
  execution_state(parent_clause(_Clause,SubtermSelector)), % Clause is as it is read from the source modulo module prefix translation, it does not have variable bindings.
  execution_state(goal(_:Goal)), % TODO consolidate, see above
  once(( % TODO With two clauses on the same line, this may find the wrong one.
      source_clause(SourceClause,File,FirstLine,LastLine,Names), % 
      FirstLine =< Line,
      LastLine >= Line)), % TODO check if SourceClause is a variant of Clause modulo module prefix normalization
  SourceClause = (_ :- SourceBody), % if we were dealing with a fact, we wouldn't be here
  select_subterm(SubtermSelector,SourceBody,SourceGoal),
  term_variables(SourceGoal,Variables), % limit our attention to the variables in the current goal, for the others we don't have the values handy
  member(Variable,Variables), % pick a variable
  member(Name=Value,Names), % pick a name
  Variable == Value, % match them
  (SourceGoal = Goal % bind current value to variable
  -> true
   ; SourceGoal = _:Goal). % in case the source form of the goal has a module prefix

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

% select_subterm(+SubtermSelector,?Term,?Subterm)
select_subterm([],Term,Term).
select_subterm([ArgNo|ArgNos],Term,Subterm) :-
  arg(ArgNo,Term,Arg),
  select_subterm(ArgNos,Arg,Subterm).

% goal_desc(+Module,+Goal,-GoalDesc)
% Used to remove the module prefix from goal descriptions if it refers to the
% type-in module.
goal_desc(Module,Goal,Goal) :-
  module(Module),
  !.
goal_desc(Module,Goal,Module:Goal).
