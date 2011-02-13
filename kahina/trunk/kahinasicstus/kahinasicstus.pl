:- module(kahinasicstus,[]).

:- multifile user:breakpoint_expansion/2.

% To trace everything using Kahina, we need:
% add_breakpoint([]-[kahina_breakpoint_action],_).

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
  execution_state(goal(Module:Goal)),	% Fails if Module:Goal is replaced with a single variable, this surely is a bug?!
  execution_state(port(Port)),
  % TODO Communicate with Kahina. Ports to consider: call, fail, redo, exit(det), exit(nondet), block, unblock; what about exception?
  write(File),nl,write(Line),nl,write(Module),nl,write(Goal),nl,write(Port),nl.
