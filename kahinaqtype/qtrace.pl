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
        use_module('$QTYPE_HOME/bitsets',[make_type/2,
                                          make_bitset/2,
                                          bs_subsumes/2]),
        use_module('$QTYPE_HOME/sign',[features/2,
                                       subtype/2,
                                       subtypes/2]) )
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

% Using rest-lists as in QType, TODO check if this hurts performance too badly.

fs_grisu(T=FL,DGs,[33,110,101,119,100,97,116,97,34,34|Grisu]) :- % !newdata"" TODO labels?
  prepare_delayed_goals_for_pp(DGs,DGFSL,Reent,1), % TODO delayed goals
  get_one_fs(T=FL),
  get_coins_to_show(T=FL,VL),
  list_to_ord_set(VL,VL1),
  list_to_ord_set(Reent,Reent1),
  ord_union(VL1,Reent1,EVL0),
  enum_coins(EVL0, EVL, 1),
  %add_dgs_to_fl(FL,DGFSL),
  empty_assoc(TD),
  fs_grisu(T=FL,EVL,TD,0,_,Grisu,[]). % first ID, close difference list

% re-entrancy tag
fs_grisu(Type=_,TL,TD,ID0,ID,[40,35|Grisu0],Grisu) :- % (#
  my_vmember((Type,Tag),TL), % FS is re-entrant
  rlmember(Tag,TD),          % has already been portrayed
  !,
  id_grisu(ID0,ID,Grisu0,[32|Grisu1]), % space
  number_grisu(Tag,Grisu1,[41|Grisu]). % )
% re-entrant FS
fs_grisu(Type=FL,TL,TD,ID0,ID,[40,82|Grisu0],Grisu) :- % (R
  my_vmember((Type,Tag),TL), % FS is re-entrant
  !,
  mark_tagged(Tag,TD),       % mark as portrayed
  id_grisu(ID0,ID1,Grisu0,[32|Grisu1]), % space
  number_grisu(Tag,Grisu1,Grisu2),
  fs_grisu_nr(Type=FL,TL,TD,ID1,ID,Grisu2,[41|Grisu]). % )
% non-re-entrant FS
fs_grisu(FS,TL,TD,ID0,ID,Grisu0,Grisu) :-
  fs_grisu_nr(FS,TL,TD,ID0,ID,Grisu0,Grisu).

% non-empty list
% TODO nel not distinguished from its subtypes in visualization.
fs_grisu_nr(Type=FL,TL,TD,ID0,ID,[40,76|Grisu0],Grisu) :- % (L
  is_nel_type(Type),
  !,
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  remove(FL,'FI':First,['RE':Rest]),
  fs_grisu(First,TL,TD,ID1,ID2,Grisu1,Grisu2),
  tail_grisu(Rest,TL,TD,ID2,ID,Grisu2,[41|Grisu]). % )
% empty list
fs_grisu_nr(Type=_,_,_,ID0,ID,[40,76|Grisu0],Grisu) :- % (L
  is_nil_type(Type),
  !,
  id_grisu(ID0,ID,Grisu0,[41|Grisu]). % )
% other feature structure
fs_grisu_nr(Type=FL,TL,TD,ID0,ID,[40,83|Grisu0],Grisu) :- % (S
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  type_grisu(Type,ID1,ID2,Grisu1,Grisu2),
  fl_grisu(FL,TL,TD,ID2,ID,Grisu2,[41|Grisu]). % )

% empty tail
tail_grisu(Type=_,_,_,ID,ID,Grisu,Grisu) :-
  is_nil_type(Type),
  !.
% non-empty tail
tail_grisu(Type=FL,TL,TD,ID0,ID,Grisu0,Grisu) :-
  is_nel_type(Type),
  !,
  remove(FL,'FI':First,['RE':Rest]),
  fs_grisu(First,TL,TD,ID0,ID1,Grisu0,Grisu1),
  tail_grisu(Rest,TL,TD,ID1,ID,Grisu1,Grisu).
% open end
tail_grisu(FS,TL,TD,ID0,ID,[40,90|Grisu0],Grisu) :- % (Z
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  fs_grisu(FS,TL,TD,ID1,ID,Grisu1,[41|Grisu]). % )

% type
type_grisu(Type,ID0,ID,[40|Grisu0],Grisu) :- % (
  id_grisu(ID0,ID,Grisu0,Grisu1),
  make_type(Type,TypeName),
  string_grisu(TypeName,Grisu1,[41|Grisu]). % )

% (open-ended) list of feature-value pairs
fl_grisu(End,_,_,ID,ID,Grisu,Grisu) :-
  var(End),
  !.
fl_grisu([F:V|Rest],TL,TD,ID0,ID,[40,86|Grisu0],Grisu) :- % (V
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  string_grisu(F,Grisu1,Grisu2),
  fs_grisu(V,TL,TD,ID1,ID2,Grisu2,Grisu3),
  fl_grisu(Rest,TL,TD,ID2,ID,Grisu3,[41|Grisu]). % )

% numeric ID
id_grisu(ID0,ID,Grisu0,Grisu) :-
  number_grisu(ID0,Grisu0,Grisu),
  ID is ID0 + 1.

% number, such as an ID or re-entrancy tag
number_grisu(Number,Grisu0,Grisu) :-
  number_codes(Number,Codes),
  open_list(Codes,Grisu0,Grisu).

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

nel_type(Nel) :-
  grammar:bs_type(list,_,List),
  grammar:bs_type(nel,_,Nel),
  bs_subsumes(List,Nel).

nil_type(Nil) :-
  grammar:bs_type(list,_,List),
  grammar:bs_type(nil,_,Nil),
  bs_subsumes(List,Nil),
  subtypes(nil,[nil]),
  features(nil,[]).  

% succeeds iff 1) lists are defined in the conventional way and 2) the type
% represented by TypeOrName is subsumed by nel and 3) its features are exactly
% FI and RE where RE has a subtype of list as type.
is_nel_type(TypeOrName) :-
  nel_type(Nel),
  make_type(TypeOrName,Type),
  bs_subsumes(Nel,Type),
  features(nel,Features),
  remove(Features,'FI':_,['RE':ReType]),
  subtype(ReType,list).

is_nil_type(Type) :-
  nil_type(Nil),
  bs_subsumes(Nil,Type).

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

% removes the first occurrence (=) of an element from a list
remove([Element|Rest],Element,Rest) :-
  !.
remove([_|Rest0],Element,Rest) :-
  remove(Element,Rest0,Rest).
remove([],_,[]).
