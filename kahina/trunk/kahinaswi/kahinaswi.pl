/*  This file is part of Kahina-SWI, a proof-of-concept graphical tracer
    for SWI-Prolog in the Kahina framework.

    Author:        Kilian Evang
    E-mail:        kevang ät sfs döt uni-tuebingen döt de
    WWW:           http://kahina.org

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(kahinaswi,[]).

:- use_module(actions,[map_action/3,
                       fix_action/3]).
:- use_module(library(jpl)).
:- use_module(pce_prolog_clause,[clear_clause_info_cache/0,
                                 pce_clause_info/4]).

:- dynamic bridge/1.
:- dynamic frame_step/2.
:- dynamic next_step/1.

start(Bridge) :-
  jpl_call(class([org,kahina,prolog],['PrologDebuggerRunner']),runAndGetBridge,
      [],Bridge),
  assert(bridge(Bridge)),
  retractall(next_step(_)), % TODO more systematic cleanup
  clear_clause_info_cache,
  assert(next_step(1)).

get_bridge(Bridge) :-
  bridge(Bridge), % TODO if that bridge is old, we must create a new one or clear it
  !.
get_bridge(Bridge) :-
  start(Bridge).

user:prolog_trace_interception(Port,Frame,_Choice,Action) :-
  % TODO check if Kahina-SWI is switched on, there should be some flag.
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
  send_bindings(Step,in,Frame,Bridge),
  jpl_call(Bridge,call,[Step],_).
act(fail,Frame,Bridge) :-
  %log(fail,Frame),
  retract(frame_step(Frame,Step)),
  jpl_call(Bridge,fail,[Step],_),
  end(Step,Bridge).
act(exit,Frame,Bridge) :-
  %log(exit,Frame),
  frame_step(Frame,Step),
  send_bindings(Step,out,Frame,Bridge),
  prolog_frame_attribute(Frame,has_alternatives,HasAlternatives),
  not(HasAlternatives,Deterministic),
  jpl_call(Bridge,exit,[Step,@(Deterministic)],_),
  end(Step,Bridge).
act(redo,Frame,Bridge) :-
  %log(redo,Frame),
  frame_step(Frame,Step),
  jpl_call(Bridge,redo,[Step],_),
  send_bindings(Step,in,Frame,Bridge).
% TODO exception ports
% TODO unify ports?
% TODO foreign ports? (are these ports?)

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

send_bindings(Step,Direction,Frame,Bridge) :-
  get_bindings(Frame,KeyList,ValueList),
  KeyList \== [],
  !,
  jpl_datums_to_array(KeyList,KeyArray),
  jpl_datums_to_array(ValueList,ValueArray),
  jpl_call(Bridge,registerBindings,[Step,Direction,KeyArray,ValueArray],_).
send_bindings(_,_,_,_).

% Credits for the following to XPCE's trace.pl...

get_bindings(Frame,VarNameList,ValueList) :-
  frame_bindings(Frame,VarNameList,ValueList),
  !.
get_bindings(Frame,ArgNumList,ValueList) :-
  frame_arguments(Frame,ArgNumList,ValueList).

frame_bindings(Frame,VarNameList,ValueList) :-
  prolog_frame_attribute(Frame,clause,Clause),
  pce_clause_info(Clause,_,_,VarNames),
  functor(VarNames,_,N),
  frame_bindings(0,N,VarNames,Frame,Bindings),
  bindings_lists(Bindings,Bindings,VarNameList,ValueList).

frame_bindings(N,N,_,_,[]) :-
  !.
frame_bindings(I,N,VarNames,Frame,[VarName:Value|Bindings]) :-
  J is I + 1,
  arg(J,VarNames,VarName),
  VarName \== '_',
  !,
  prolog_frame_attribute(Frame,argument(J),Value),
  frame_bindings(J,N,VarNames,Frame,Bindings).
frame_bindings(I,N,VarNames,Frame,Bindings) :-
  J is I + 1,
  frame_bindings(J,N,VarNames,Frame,Bindings).

frame_arguments(Frame,ArgNumList,ValueList) :-
  prolog_frame_attribute(Frame,goal,Goal),
  functor(Goal,_,N),
  frame_arguments(0,N,Frame,ArgNumList,ValueList).

frame_arguments(N,N,_,[],[]) :-
  !.
frame_arguments(I,N,Frame,[ArgNumAtom|ArgNumList],[ValueAtom|ValueList]) :-
  J is I + 1,
  term_to_atom(J,ArgNumAtom),
  prolog_frame_attribute(Frame,argument(J),Value),
  term_to_atom(Value,ValueAtom),
  frame_arguments(J,N,Frame,ArgNumList,ValueList).

bindings_lists([],_,[],[]).
bindings_lists([_:Value|BindingsRest],Bindings,VarNames,ValueAtoms) :-
  var(Value),
  !,
  bindings_lists(BindingsRest,Bindings,VarNames,ValueAtoms).
bindings_lists([VarName:Value|BindingsRest],Bindings,[VarName|VarNames],[ValueAtom|ValueAtoms]) :-
  insert_varnames(Value,Bindings,Value1),
  with_output_to(atom(ValueAtom),write_term(Value1,[numbervars(true)])),
  bindings_lists(BindingsRest,Bindings,VarNames,ValueAtoms).

insert_varnames(Term,Bindings,Result) :-
  var(Term),
  !,
  insert_varnames_var(Term,Bindings,Result).
insert_varnames(Term,Bindings,Result) :-
  functor(Term,Functor,N),
  functor(Result,Functor,N),
  insert_varnames_args(0,N,Term,Bindings,Result).

insert_varnames_args(N,N,_,_,_) :-
  !.
insert_varnames_args(I,N,Term,Bindings,Result) :-
  J is I + 1,
  arg(J,Term,Argument),
  arg(J,Result,ResultArgument),
  insert_varnames(Argument,Bindings,ResultArgument),
  insert_varnames_args(J,N,Term,Bindings,Result).

insert_varnames_var(Var,Bindings,'$VAR'(VarName)) :- % for write_term/2
  member(VarName:Var2,Bindings),
  Var == Var2,
  !.
insert_varnames_var(Var,_,Var).

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
