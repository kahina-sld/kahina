:- module(qtrace,[qtrace/0,
                  noqtrace/0,
                  source_code_location/2]).

:- use_module('../kahinasicstus/kahinasicstus').
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(terms)).

% TODO make sure the root (step 0) gets an exit call so the GUI updates

% ------------------------------------------------------------------------------
% STARTUP TEST
% ------------------------------------------------------------------------------

:- environ('QTYPE_HOME',_)
   -> ( use_module('$QTYPE_HOME/atts'),
        use_module('$QTYPE_HOME/ops'),
        use_module('$QTYPE_HOME/auxlib',[default_extension/3]),
        use_module('$QTYPE_HOME/bitsets',[make_bitset/2,
                                          make_type/2]))
    ; raise_exception(qtype_home_not_set).

% ------------------------------------------------------------------------------
% HOOK IMPLEMENTATIONS
% ------------------------------------------------------------------------------

user:generate_message_hook(qtype_home_not_set,[format('ERROR: Environment variable QTYPE_HOME must be set to path of directory',[]),nl,format('containing qtype.pl',[]),nl|Tail],Tail).

% TODO The following should check if we really are in a qtrace session, or maybe
% there is a better way to customize the tracer behavior than hooks. Currently,
% loading this module renders Kahina for SICStus unusable (which is not that
% much of an issue, but still...)

:- multifile kahinasicstus:abort_hook/2.

% We want to return to QType prompt, not SICStus prompt:
kahinasicstus:abort_hook(trace,raise(kahinaqtype_abort)).

:- multifile kahinasicstus:breakpoint_action_hook/4.

% This exception has a special meaning and should not be traced:
kahinasicstus:breakpoint_action_hook(exception(kahinaqtype_abort),_,debug,proceed,_).

:- multifile kahinasicstus:instance_class_hook/1.

kahinasicstus:instance_class_hook('org/kahina/qtype/QTypeDebuggerInstance').

% ------------------------------------------------------------------------------
% PUBLIC PREDICATES
% ------------------------------------------------------------------------------

:- dynamic qbreakpoint/2. % qbreakpoint(Module:Functor/Arity,BID)

% First approximation to a QType-specific tracer:
% Sets a breakpoint with kahina_breakpoint_action on every QType predicate with
% at least one clause that has an immediate msg/3 subgoal.
qtrace :-
  qtype_home(Home),
  set_breakpoints(Home),
  debug.

noqtrace :-
  retract(qbreakpoint(_,BID)),
  remove_breakpoints([BID]),
  fail.
noqtrace.

source_code_location(File,Line) :-
  execution_state(goal(Module:Goal)),
  goal_source_code_location(Module:Goal,File,Line).

% ------------------------------------------------------------------------------
% SETTING BREAKPOINTS FOR QTYPE
% ------------------------------------------------------------------------------

% always succeeds
add_qbreakpoint(Pred,Options) :-
  \+ qbreakpoint(Pred,_),
  !,
  add_breakpoint([pred(Pred),(call;fail;exit;redo;exception;block;unblock)]-[kahina_breakpoint_action(Options)],BID),
  assert(qbreakpoint(Pred,BID)).
add_qbreakpoint(_,_).

% failure-driven
set_breakpoints(_) :-
  add_qbreakpoint(parser:lc/1,[]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(parser:lc/2,[]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(parser:lc/5,[]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(parser:lc_complete/8,[]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(parser:lc_list/5,[]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(grammar:db_word/4,[source_code_location(qtrace:source_code_location(File,Line),File,Line)]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(grammar:db_rule/4,[source_code_location(qtrace:source_code_location(File,Line),File,Line)]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(grammar:db_macro/5,[source_code_location(qtrace:source_code_location(File,Line),File,Line)]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(lexrules:apply_lexrules/4,[source_code_location(qtrace:source_code_location(File,Line),File,Line)]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(grammar:db_lexrule/4,[source_code_location(qtrace:source_code_location(File,Line),File,Line)]),
  fail.
set_breakpoints(Home) :-
  directory_files(Home,Files),
  atom_codes('.pl',ExtensionCodes),
  member(File,Files),
  \+ not_parsed(File),
  atom_codes(File,FileCodes),
  ends_with(FileCodes,ExtensionCodes),
  absolute_file_name(File,AbsFile,[relative_to(Home)]),
  open(AbsFile,read,Stream,[eof_action(eof_code)]),
  set_breakpoints_file(Stream,_),
  close(Stream),
  fail.
set_breakpoints(_).

not_parsed('atts.pl').
not_parsed('ops.pl').

% recursive, always succeeds deterministically
set_breakpoints_file(Stream,ModuleHint) :-
  read(Stream,Term),
  set_breakpoints_term(Term,ModuleHint),
  !,
  set_breakpoints_file(Stream,ModuleHint).
set_breakpoints_file(_,_).

% fails on end of file
set_breakpoints_term((:- module(ModuleHint)),ModuleHint) :-
  !.
set_breakpoints_term((:- module(ModuleHint,_)),ModuleHint) :-
  !.
set_breakpoints_term(end_of_file,_) :-
  !,
  fail.
set_breakpoints_term((Head :- Body),ModuleHint) :-
  !,
  hint_module(ModuleHint,Module),
  set_breakpoints_clause(Module,Head,Body).  
set_breakpoints_term(_,_).

% always succeeds
set_breakpoints_clause(Module,Head,Body) :-
  module_head_pred(Module,Head,Pred),
  \+ not_traced(Pred),
  has_msg(Body,_Layer),
  !,
  autoskip(Pred,Autoskip),
  add_qbreakpoint(Pred,[autoskip(Autoskip)]).
set_breakpoints_clause(_,_,_).

% ------------------------------------------------------------------------------
% DATA
% ------------------------------------------------------------------------------

not_traced(timer:msg_timer/_).
not_traced(timer:msg_timer2/_).
not_traced(cmd_aux:call_prolog/1).
%not_traced(cp_exsyn:tokenize_and_parse/0). % TODO rules, constraints, macros etc. views?
%not_traced(cp_sig:define_undefined_subtypes/1). % TODO type hierarchy view with features?
%not_traced(cp_sig:connect_homeless_types/2).
%not_traced(cp_sig:connect_homeless_types/3).
%not_traced(cp_sig:m_t_d/1).
%not_traced(cp_sig:is_this_atom/1).
%not_traced(cp_sig:cp_flubs/1).
%not_traced(cp_sig:expand_type/2)
%not_traced(bitsets:_/_).

autoskip(_,false).

goal_source_code_location(grammar:db_word(_,_,_,Line),File,Line) :-
  line_source_code_location(File,Line).
goal_source_code_location(grammar:db_rule(_,_,_,Line),File,Line) :-
  line_source_code_location(File,Line).
goal_source_code_location(grammar:db_macro(_,_,_,_,Line),File,Line) :-
  line_source_code_location(File,Line).
goal_source_code_location(lexrule:apply_lexrules(_,_,_,Line),File,Line) :-
  line_source_code_location(File,Line).
goal_source_code_location(grammar:db_lexrule(_,_,_,Line),File,Line) :-
  line_source_code_location(File,Line).

% ------------------------------------------------------------------------------
% FEATURE STRUCTURES
% Converting features structures from QType's term representation to the Grisu
% format understood by Gralej.
% ------------------------------------------------------------------------------

fs_grisu(FS,[33,110,101,119,100,97,116,97,34,34|Grisu]) :- % !newdata"" TODO labels?
  fs_grisu(FS,0,_,Grisu,[]). % first ID, close difference list

% The last two arguments of the following predicates represent difference lists,
% used to recursively construct Grisu messages.

% feature structure
fs_grisu(Type=FVPs,ID0,ID,[40,83|Grisu0],Grisu) :- % (S
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  type_grisu(Type,ID1,ID2,Grisu1,Grisu2),
  fvps_grisu(FVPs,ID2,ID,Grisu2,[41|Grisu]). % )

% type
type_grisu(Type,ID0,ID,[40|Grisu0],Grisu) :- % (
  id_grisu(ID0,ID,Grisu0,Grisu1),
  make_bitset(Type,Bitset), % now typically, Type==Bitset
  make_type(Bitset,TypeName),
  string_grisu(TypeName,Grisu1,[41|Grisu]). % )

% (open-ended) list of feature-value pairs
fvps_grisu(End,ID,ID,Grisu,Grisu) :-
  var(End),
  !.
fvps_grisu([F:V|Rest],ID0,ID,[40,86|Grisu0],Grisu) :- % (V
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  string_grisu(F,Grisu1,Grisu2),
  fs_grisu(V,ID1,ID2,Grisu2,Grisu3),
  fvps_grisu(Rest,ID2,ID,Grisu3,[41|Grisu]). % )

% numeric ID
id_grisu(ID0,ID,Grisu0,Grisu) :-
  number_codes(ID0,ID0Codes),
  open_list(ID0Codes,Grisu0,Grisu),
  ID is ID0 + 1.

% string, such as a type or feature name
% the character " may not appear in Grisu string literals, at least
% gralej.parser.TraleMsgLexer doesn't support it. So no escaping here.
string_grisu(Atom,[34|Grisu0],Grisu) :- % "
  atom_codes(Atom,Codes),
  open_list(Codes,Grisu0,[34|Grisu]). % "

% No special treatment for atoms needed - they are just types with no features
% (thus empty features lists), conventionally immediate subtypes of the built-in
% type atom and inferred by QType from usage in the grammar rather than
% declared.

% ------------------------------------------------------------------------------
% HELPERS (KAHINAQTYPE-SPECIFIC)
% ------------------------------------------------------------------------------

has_msg(msg(_,Level,_),Layer) :-
  clean_level(Level,Layer).
has_msg((msg(_,Level,_),Rest),Layer) :-
  clean_level(Level,CleanLevel),
  !,
  determine_layer(CleanLevel,Rest,Layer).
has_msg((_,Rest),Layer) :-
  has_msg(Rest,Layer).

determine_layer(LowestLevel,(First,Rest),Layer) :-
 !,
 lowest_msg_level(LowestLevel,First,NewLowestLevel),
 determine_layer(NewLowestLevel,Rest,Layer).
determine_layer(LowestLevel,Last,Layer) :-
  lowest_msg_level(LowestLevel,Last,Layer).

lowest_msg_level(LowestLevel,msg(_,Level,_),NewLowestLevel) :-
  clean_level(Level,CleanLevel),
  !,
  NewLowestLevel is min(LowestLevel,CleanLevel).
lowest_msg_level(Level,_,Level).

clean_level(@Level,Level) :-
  !,
  integer(Level).
clean_level(Level,Level) :-
  integer(Level).

qtype_home(Home) :-
  environ('QTYPE_HOME',Home),
  !.
qtype_home(_) :-
  raise_exception(qtype_home_not_set).

line_source_code_location(File,Line) :-
  nonvar(Line),
  clause(cmd_aux:current_compilefile(File0),_),
  default_extension(File0,grm,File).

% ------------------------------------------------------------------------------
% HELPERS (GENERAL)
% ------------------------------------------------------------------------------

module_head_pred(_,Module:Head,Module:Functor/Arity) :-
  !,
  functor(Head,Functor,Arity).
module_head_pred(Module,Head,Module:Functor/Arity) :-
  functor(Head,Functor,Arity).

ends_with(List,List) :-
  !.
ends_with([_|Rest],Suffix) :-
  ends_with(Rest,Suffix).

hint_module(Hint,user) :-
  var(Hint),
  !.
hint_module(Module,Module).

% proper list -> difference list
open_list([First|Rest],[First|NewRest],End) :-
  open_list(Rest,NewRest,End).
open_list([],End,End).
