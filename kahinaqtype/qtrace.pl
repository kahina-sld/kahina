:- module(qtrace,[qtrace/0,
                  noqtrace/0,
                  source_code_location/2,
                  cmdline/1]).

:- use_module('../kahinasicstus/kahinasicstus').
:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(jasper)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(system)).
:- use_module(library(terms)).

% TODO make sure the root (step 0) gets an exit call so the GUI updates

% ------------------------------------------------------------------------------
% STARTUP TEST
% ------------------------------------------------------------------------------

% TODO many of these imported predicates are private, get rid of warnings... one
% way or another...
:- environ('QTYPE_HOME',_)
   -> ( use_module('$QTYPE_HOME/atts'),
        use_module('$QTYPE_HOME/ops'),
        use_module('$QTYPE_HOME/auxlib',[default_extension/3]),
        use_module('$QTYPE_HOME/bitsets',[make_type/2,
                                          bs_subsumes/2,
                                          is_var_and_attr/1,
                                          is_virtual_type/1,
                                          make_bitset/2]),
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
kahinasicstus:abort_hook(trace,raise(tracer_abort)).

:- multifile kahinasicstus:breakpoint_action_hook/5.

% This exception has a special meaning and should not be traced:
kahinasicstus:breakpoint_action_hook(exception(tracer_abort),_,debug,proceed,_).

:- multifile kahinasicstus:instance_class_hook/1.

kahinasicstus:instance_class_hook('org/kahina/qtype/QTypeDebuggerInstance').

:- multifile kahinasicstus:post_step_hook/5.

kahinasicstus:post_step_hook(Bridge,JVM,Inv,_,_) :-
  execution_state(goal(_:Goal)),
  term_grisu(Goal,'',Grisu),
  register_goal(Bridge,JVM,Inv,[105,110],Grisu). % in

:- multifile kahinasicstus:post_exit_hook/5.

kahinasicstus:post_exit_hook(Bridge,JVM,Inv,_,_) :-
  execution_state(goal(_:Goal)),
  term_grisu(Goal,'',Grisu),
  register_goal(Bridge,JVM,Inv,[111,117,116],Grisu). % out

register_goal(Bridge,JVM,Inv,KeyChars,Grisu) :-
  jasper_call(JVM,
      method('org/kahina/qtype/bridge/QTypeBridge','registerGoal',[instance]),
      register_goal(+object('org/kahina/qtype/bridge/QTypeBridge'),+integer,+chars,+chars),
      register_goal(Bridge,Inv,KeyChars,Grisu)).

:- multifile kahinasicstus:classpath_element/1.

kahinasicstus:classpath_element('$KAHINA_HOME/lib/gralej/gralej.jar'). % TODO check for $KAHINA_HOME
kahinasicstus:classpath_element('$KAHINA_HOME/lib/gralej/lib/tomato.jar').

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

cmdline(ASCII) :-
  get_jvm(JVM),
  get_kahina_instance(Instance,JVM),
  jasper_call(JVM,
      method('org/kahina/qtype/QTypeDebuggerInstance','getCommand',[instance]),
      get_command(+object('org/kahina/qtype/QTypeDebuggerInstance'),[-chars]),
      get_command(Instance,ASCII)).

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
set_breakpoints(_) :-
  add_qbreakpoint(descr:start_constraint/1,[source_code_location(qtrace:source_code_location(File,Line),File,Line)]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(grammar:lexample/2,[source_code_location(qtrace:source_code_location(File,Line),File,Line)]),
  fail.
set_breakpoints(_) :-
  add_qbreakpoint(cp_exsyn:gram_entry/4,[source_code_location(qtrace:source_code_location(File,Line),File,Line)]),
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
not_traced(cmd_aux:print_a_string/1).
not_traced(cmd_aux:print_a_string2/1).
not_traced(tokenize:issue_warning/1).
%not_traced(cp_exsyn:tokenize_and_parse/0).
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
goal_source_code_location(descr:start_constraint(Line),File,Line) :-
  line_source_code_location(File,Line).
goal_source_code_location(grammar:lexample(Line,_),File,Line) :-
  line_source_code_location(File,Line).
goal_source_code_location(cp_exsyn:gram_entry(Line,_,_,_),File,Line) :- % TODO do the same for cp_exsyn:parse_entry/2, but that is still an automatically generated breakpoint
  line_source_code_location(File,Line).

% ------------------------------------------------------------------------------
% FEATURE STRUCTURES
% Converting features structures from QType's term representation to the Grisu
% format understood by Gralej.
% ------------------------------------------------------------------------------

% term_grisu(+Term,+Label,-Grisu)
%   Term: A Prolog term. May be a QType FS or contain such.
%   Label: A label for output, given as an atom.
%   Grisu: A code-list representing the Grisu message portraying the term up to
%          a depth of 5 (this limit does not apply to FSs).
term_grisu(Term,Label,[33,110,101,119,100,97,116,97|Grisu0]) :- % !newdata
  string_grisu(Label,Grisu0,Grisu1),
  term_coins(Term,Coins),
  term_grisu(Term,5,Coins,0,ID1,Grisu1,Grisu2),
  coins_grisu(Coins,ID1,_,Grisu2,[10]). % LF; close list

% Collect re-entrancies, or "coins", as in "coindexed":

% term_coins(?Term,-Coins)
% Coins is an assoc mapping every FS that occurs more than once in Term to a
% unique number, starting from 1.
term_coins(Term,Coins) :-
  empty_assoc(Empty),
  term_coins(Term,1,_,Empty,_,Empty,Coins).

term_coins(Term,Tag,Tag,Seen,Seen,Coins,Coins) :-
  var(Term),
  !.
term_coins(Term,Tag,Tag,Seen,Seen,Coins,Coins) :-
  get_assoc(Term,Seen,_),
  !.
term_coins(Term,Tag0,Tag,Seen0,Seen,Coins0,Coins) :-
  is_fs(Term),
  !,
  fs_coins(Term,Tag0,Tag,Seen0,Seen,Coins0,Coins).
term_coins(Term,Tag0,Tag,Seen0,Seen,Coins0,Coins) :-
  Term =.. [_|Args],
  arglist_coins(Args,Tag0,Tag,Seen0,Seen,Coins0,Coins).

arglist_coins([],Tag,Tag,Seen,Seen,Coins,Coins).
arglist_coins([First|Rest],Tag0,Tag,Seen0,Seen,Coins0,Coins) :-
  term_coins(First,Tag0,Tag1,Seen0,Seen1,Coins0,Coins1),
  arglist_coins(Rest,Tag1,Tag,Seen1,Seen,Coins1,Coins).

fs_coins(T=_,Tag,Tag,Seen,Seen,Coins,Coins) :-
  get_assoc(T,Coins,_),
  !.
fs_coins(T=FL,Tag0,Tag,Seen0,Seen,Coins0,Coins) :-
  del_assoc(T,Seen0,_,Seen),
  !,
  put_assoc(T,Coins0,coin(Tag0,T=FL),Coins),
  Tag is Tag0 + 1.
fs_coins(T=FL,Tag0,Tag,Seen0,Seen,Coins0,Coins) :-
  put_assoc(T,Seen0,_,Seen1),
  fl_coins(FL,Tag0,Tag,Seen1,Seen,Coins0,Coins).

fl_coins(FL,Tag,Tag,Seen,Seen,Coins,Coins) :-
  var(FL),
  !.
fl_coins([_:V|Rest],Tag0,Tag,Seen0,Seen,Coins0,Coins) :-
  fs_coins(V,Tag0,Tag1,Seen0,Seen1,Coins0,Coins1),
  fl_coins(Rest,Tag1,Tag,Seen1,Seen,Coins1,Coins).

% The actual portraying:

% variable
term_grisu(Term,_Depth,_Coins,ID0,ID,Grisu0,Grisu) :-
  var(Term),
  !,
  var_grisu(Term,ID0,ID,Grisu0,Grisu).
% FS
term_grisu(Term,_Depth,Coins,ID0,ID,Grisu0,Grisu) :-
  should_be_attempted_to_portray_as_fs(Term), % looks like a good FS on the surface
  fs_grisu(Term,Coins,ID0,ID,Grisu0,Grisu), % may still fail, naughty FSs may be hidden inside
  !.
% number
term_grisu(Term,_Depth,_Coins,ID0,ID,Grisu0,Grisu) :-
  number(Term),
  !,
  numberliteral_grisu(Term,ID0,ID,Grisu0,Grisu).
% empty list
term_grisu([],_Depth,_Coins,ID0,ID,Grisu0,Grisu) :-
  !,
  elist_grisu(ID0,ID,Grisu0,Grisu).
% non-empty list
term_grisu([Head|Tail],Depth,Coins,ID0,ID,Grisu0,Grisu) :-
  !,
  nelist_grisu(Head,Tail,Depth,Coins,ID0,ID,Grisu0,Grisu).
% other terms
term_grisu(Term,Depth,Coins,ID0,ID,[40,68|Grisu0],Grisu) :- % (D
  Term =.. [Functor|Args], 
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  string_grisu(Functor,Grisu1,Grisu2),
  arglist_grisu(Args,Depth,Coins,ID1,ID,Grisu2,[41|Grisu]). % )

elist_grisu(ID0,ID,[40,76|Grisu0],Grisu) :- % (L
  id_grisu(ID0,ID,Grisu0,[41|Grisu]). % )

nelist_grisu(_,_,0,_,ID0,ID,[40,76|Grisu0],Grisu) :- % (L
  !,
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  ellipsis_grisu(ID1,ID,Grisu1,[41|Grisu]). % )
nelist_grisu(Head,Tail,Depth,Coins,ID0,ID,[40,76|Grisu0],Grisu) :- % (L
  dec(Depth,NewDepth),
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  term_grisu(Head,NewDepth,Coins,ID1,ID2,Grisu1,Grisu2),
  tail_grisu(Tail,NewDepth,Coins,ID2,ID,Grisu2,[41|Grisu]). % )

% variable tail
tail_grisu(Tail,Depth,Coins,ID0,ID,[40,90|Grisu0],Grisu) :- % (Z
  var(Tail),
  !,
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  term_grisu(Tail,Depth,Coins,ID1,ID,Grisu1,[41|Grisu]). % )
% empty tail
tail_grisu([],_,_,ID,ID,Grisu,Grisu) :-
  !.
% depth limit
tail_grisu(_,0,_,ID0,ID,[40,90|Grisu0],Grisu) :- % (Z
  !,
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  ellipsis_grisu(ID1,ID,Grisu1,[41|Grisu]). % )
% non-empty tail
tail_grisu([First|Rest],Depth,Coins,ID0,ID,Grisu0,Grisu) :-
  !,
  dec(Depth,NewDepth),
  term_grisu(First,NewDepth,Coins,ID0,ID1,Grisu0,Grisu1),
  tail_grisu(Rest,NewDepth,Coins,ID1,ID,Grisu1,Grisu).
% non-variable non-list tail
tail_grisu(Tail,Depth,Coins,ID0,ID,[40,90|Grisu0],Grisu) :- % (Z
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  term_grisu(Tail,Depth,Coins,ID1,ID,Grisu1,[41|Grisu]). % )

arglist_grisu([],_,_,ID,ID,Grisu,Grisu).
arglist_grisu([_|_],0,_,ID0,ID,Grisu0,Grisu) :-
  !,
  ellipsis_grisu(ID0,ID,Grisu0,Grisu).
arglist_grisu([Arg|Args],Depth,Coins,ID0,ID,Grisu0,Grisu) :-
  dec(Depth,NewDepth),
  term_grisu(Arg,NewDepth,Coins,ID0,ID1,Grisu0,Grisu1),
  arglist_grisu(Args,Depth,Coins,ID1,ID,Grisu1,Grisu).

% re-entrancy tag
fs_grisu(T=_,Coins,ID0,ID,[40,35|Grisu0],Grisu) :- % (#
  get_assoc(T,Coins,coin(Tag,_)),
  !,
  id_grisu(ID0,ID,Grisu0,[32|Grisu1]), % space
  number_grisu(Tag,Grisu1,[41|Grisu]). % )
fs_grisu(FS,Coins,ID0,ID,Grisu0,Grisu) :-
  fs_grisu_nr(FS,Coins,ID0,ID,Grisu0,Grisu).

% non-empty list
% TODO nel not distinguished from its subtypes in visualization.
fs_grisu_nr(T=FL,Coins,ID0,ID,[40,76|Grisu0],Grisu) :- % (L
  is_nel_type(T),
  !,
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  % Fun fact about QType's FS representation: for an FS T=FL, appropriate
  % features for T need not (yet) be in FL (for now, we use a Prolog variable
  % returned by fl_feature/3 to display unknown FI/RE values), and conversely,
  % features in FL need not be appropriate for T (T is then more general than a
  % type that introduces the feature). The latter is why not every list FS is
  % displayed as a list. E.g. we could have an scl-type FS with a FI feature -
  % it clearly represents a non-empty list, but the type does not say so. We
  % could of course automagically infer it from the presence of the FI feature
  % and from the fact that scl (in this example) is a subtype of list, but in
  % the absence of strong reasons to do so I would prefer to more explicitly
  % represent the underlying structure in such cases.
  fl_feature(FL,'FI',First),
  fl_feature(FL,'RE',Rest),
  term_grisu(First,5,Coins,ID1,ID2,Grisu1,Grisu2),
  fs_tail_grisu(Rest,Coins,ID2,ID,Grisu2,[41|Grisu]). % )
% empty list
fs_grisu_nr(T=_,_,ID0,ID,[40,76|Grisu0],Grisu) :- % (L
  is_nil_type(T),
  !,
  id_grisu(ID0,ID,Grisu0,[41|Grisu]). % )
% other feature structure
fs_grisu_nr(T=FL,Coins,ID0,ID,[40,83|Grisu0],Grisu) :- % (S
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  type_grisu(T,ID1,ID2,Grisu1,Grisu2),
  fl_grisu(FL,Coins,ID2,ID,Grisu2,[41|Grisu]). % )

% re-entrant FS tail
fs_tail_grisu(Tail,Coins,ID0,ID,[40,90,40,35|Grisu0],Grisu) :- % (Z(#
  get_assoc(Tail,Coins,coin(Tag,_)),
  !,
  id_grisu(ID0,ID,Grisu0,[32|Grisu1]), % space
  number_grisu(Tag,Grisu1,[41,41|Grisu]). % ))
% variable tail
fs_tail_grisu(Tail,_,ID0,ID,[40,90|Grisu0],Grisu) :- % (Z
  var(Tail),
  !,
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  var_grisu(Tail,ID1,ID,Grisu1,[41|Grisu]). % )
% empty list FS tail
fs_tail_grisu(T=_,_,ID,ID,Grisu,Grisu) :-
  is_nil_type(T),
  !.
% non-empty list FS tail
fs_tail_grisu(T=FL,Coins,ID0,ID,Grisu0,Grisu) :-
  is_nel_type(T),
  !,
  fl_feature(FL,'FI',First),
  fl_feature(FL,'RE',Rest),
  term_grisu(First,5,Coins,ID0,ID1,Grisu0,Grisu1),
  fs_tail_grisu(Rest,Coins,ID1,ID,Grisu1,Grisu).
% non-list FS tail
fs_tail_grisu(FS,Coins,ID0,ID,[40,90|Grisu0],Grisu) :- % (Z
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  fs_grisu(FS,Coins,ID1,ID,Grisu1,[41|Grisu]).

% type
type_grisu(T,ID0,ID,[40|Grisu0],Grisu) :- % (
  id_grisu(ID0,ID,Grisu0,Grisu1),
  make_type(T,TypeName),
  string_grisu(TypeName,Grisu1,[41|Grisu]). % )

% (open-ended) list of feature-value pairs
fl_grisu(End,_,ID,ID,Grisu,Grisu) :-
  var(End),
  !.
fl_grisu([F:V|Rest],Coins,ID0,ID,[40,86|Grisu0],Grisu) :- % (V
  id_grisu(ID0,ID1,Grisu0,Grisu1),
  string_grisu(F,Grisu1,Grisu2),
  term_grisu(V,5,Coins,ID1,ID2,Grisu2,[41|Grisu3]), % )
  fl_grisu(Rest,Coins,ID2,ID,Grisu3,Grisu).

% ID (increments ID counter)
id_grisu(ID0,ID,Grisu0,Grisu) :-
  number_grisu(ID0,Grisu0,Grisu),
  ID is ID0 + 1.

% number, such as an ID or re-entrancy tag
number_grisu(Number,Grisu0,Grisu) :-
  number_codes(Number,Codes),
  open_list(Codes,Grisu0,Grisu).

var_grisu(Var,ID0,ID,[40,65|Grisu0],Grisu) :- % (A
  id_grisu(ID0,ID,Grisu0,[34|Grisu1]), % "
  write_to_chars(Var,Codes),
  open_list(Codes,Grisu1,[34,41|Grisu]). % ")

numberliteral_grisu(Number,ID0,ID,[40,65|Grisu0],Grisu) :- % (A
  id_grisu(ID0,ID,Grisu0,[34|Grisu1]), % "
  number_codes(Number,Codes),
  open_list(Codes,Grisu1,[34,41|Grisu]). % ")

% string, such as a type, feature or functor
% the character " may not appear in Grisu string literals, at least
% gralej.parser.TraleMsgLexer doesn't support it. So no escaping here.
string_grisu(Atom,[34|Grisu0],Grisu) :- % "
  atom_codes(Atom,Codes),
  open_list(Codes,Grisu0,[34|Grisu]). % "

ellipsis_grisu(ID0,ID,[40,65|Grisu0],Grisu) :- % (A
  id_grisu(ID0,ID,Grisu0,[34,46,46,46,34,41|Grisu]). % "...")

% No special treatment for atoms needed - they are just types with no features
% (thus empty feature lists), conventionally immediate subtypes of the built-in
% type atom and inferred by QType from usage in the grammar rather than
% declared.

% Portraying re-entrant FSs:

coins_grisu(Coins,ID0,ID,Grisu0,Grisu) :-
  assoc_to_list(Coins,CoinList),
  coinlist_grisu(CoinList,Coins,ID0,ID,Grisu0,Grisu).

coinlist_grisu([],_,ID,ID,Grisu,Grisu).
coinlist_grisu([_-coin(Tag,FS)|Rest],Coins,ID0,ID,Grisu0,Grisu) :-
  coin_grisu(FS,Tag,Coins,ID0,ID1,Grisu0,Grisu1),
  coinlist_grisu(Rest,Coins,ID1,ID,Grisu1,Grisu).

coin_grisu(FS,Tag,Coins,ID0,ID,[40,82|Grisu0],Grisu) :- % (R
  id_grisu(ID0,ID1,Grisu0,[32|Grisu1]), % space
  number_grisu(Tag,Grisu1,Grisu2),
  fs_grisu_nr(FS,Coins,ID1,ID,Grisu2,[41|Grisu]). % )

% distinguishing FSs from other terms

should_be_attempted_to_portray_as_fs(Term) :-
  nonvar(Term),
  Term = (T=_),
  is_var_and_attr(T),
  \+ is_virtual_type(T).

is_fs(Term) :-
  nonvar(Term),
  Term = (T=_),
  is_var_and_attr(T).

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
  make_bitset(TypeOrName,Type),
  bs_subsumes(Nel,Type),
  features(nel,Features),
  remove(Features,'FI':_,['RE':ReType]),
  subtype(ReType,list).

is_nil_type(Type) :-
  nil_type(Nil),
  bs_subsumes(Nil,Type).

fl_feature(FL,FeatureName,Value) :-
  remove(FL,FeatureName:Value,_),
  !.
fl_feature(_,_,_).

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

% Removes the first occurrence (=) of an element from a list, if any. The list
% may be open-ended.
remove(List,_,List) :-
  var(List),
  !.
remove([Element|Rest],Element,Rest) :-
  !.
remove([First|Rest0],Element,[First|Rest]) :-
  remove(Rest0,Element,Rest).
remove([],_,[]).

dec(infinity,infinity) :-
  !.
dec(M,N) :-
  N is M - 1.
