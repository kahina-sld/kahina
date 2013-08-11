% Multifile declarations.
:- multifile if/2.
:- multifile forall/2.
%:- load_cd.

%:- [trale_home(feature_ordering)].

% signature file
signature(signature).

% GRALE output specifications

hidden_feat(daughters).    % shown by the tree

synsem <<< arg_st.
vform <<< aux.
vform <<< inv.
subj <<< comps.
liker <<< liked.
sayer <<< said.
giver <<< gift.
gift <<< given.
>>> phon.


%====================================================================

% Simulation of Parametric Sorts as Principles of Grammar

fun list_of_synsems(-).

list_of_synsems(X) if
   when( (X=(e_list;ne_list) ),
           und_list_of_synsems(X)).

und_list_of_synsems(X) if (X=e_list).
und_list_of_synsems(X) if (X=[(synsem)|Y]),list_of_synsems(Y).

% subj:list(synsem)
% comps:ist(synsem)

val *> (subj:list_of_synsems,
        comps:list_of_synsems).

% arg_st:list(synsem)
word *> arg_st:list_of_synsems.


fun list_of_locs(-).

list_of_locs(X) if
   when( (X=(e_list;ne_list) ),
           und_list_of_locs(X)).

und_list_of_locs(X) if (X=e_list).
und_list_of_locs(X) if (X=[(loc)|Y]),list_of_locs(Y).


% inherited:list(loc)
% to_bind:list(loc)

nonloc *> (inherited:list_of_locs,
           to_bind:list_of_locs).


%====================================================================
% Phrase Structure Rules, encoding the ID-Principle and the
%                                      Constituent Order Principle and
%                                      Subcategorization Principle


% Head Subject Rule

head_subject_rule rule
(phrase,
 synsem:loc:cat:val:(subj:e_list,
                     comps:e_list),
 daughters:(hs_struc,
            hdtr:Hdtr,
            ndtr:Ndtr),
 phon:MotherPhon)
   ===>
cat> (Ndtr, synsem:Synsem, phon:NdtrPhon),
cat> (Hdtr, synsem:loc:cat:val:(subj:[Synsem],
                                comps:e_list),
      phon:HdtrPhon),
goal> append(NdtrPhon,HdtrPhon,MotherPhon).


% Head Complement Rule

head_complement_rule rule
(phrase,
 synsem:(loc:cat:val:(subj:Subj,
                     comps:List)),
 daughters:(hc_struc,
            hdtr:Hdtr,
            ndtr:Ndtr),
 phon:MotherPhon)
   ===>
cat> (Hdtr, synsem:loc:cat:val:(subj:Subj,
                                comps:[Synsem|List]),
      phon:HdtrPhon),
cat> (Ndtr, synsem:Synsem, phon:NdtrPhon),
goal> append(HdtrPhon,NdtrPhon,MotherPhon).


% Head Adjunct Rule

head_adjunct_rule rule
(phrase,
 synsem:loc:cat:val:(subj:List,
                      comps:e_list),
 daughters:(ha_struc,
            hdtr:Hdtr,
            ndtr:Ndtr),
 phon:MotherPhon)
   ===>
cat> (Hdtr, synsem:(Synsem,
                   loc:cat:val:(subj:List,
                                comps:e_list)),
      phon:HdtrPhon),
cat> (Ndtr, synsem:loc:cat:(head:mod:Synsem,
                            val:(subj:e_list,
                                 comps:e_list)),  % added
      phon:NdtrPhon),
goal> append(HdtrPhon,NdtrPhon,MotherPhon).


% Subject Aux Inversion Rule

subject_aux_inversion_rule rule
(phrase,
 synsem:(loc:cat:val:(subj:e_list,
		  comps:List)),
 daughters:(sai_struc,
            hdtr:Hdtr,
            ndtr:Ndtr),
 phon:MotherPhon)
   ===>
cat> (Hdtr, word, 
      synsem:loc:cat:(head:inv:plus,
                      val:(subj:[Synsem],
                           comps:List)),
      phon:HdtrPhon),
cat> (Ndtr, synsem:Synsem, phon:NdtrPhon),
goal> append(HdtrPhon,NdtrPhon,MotherPhon).


% Head Filler Rule

head_filler_rule rule
(phrase,
 synsem:loc:cat:val:Val,
 daughters:(hf_struc,
            hdtr:Hdtr,
            ndtr:Ndtr),
 phon:MotherPhon)
   ===>
cat> (Ndtr, phon:ne_list,
            synsem:(loc:Local,
                    nonloc:inherited:e_list),
      phon:NdtrPhon),
cat> (Hdtr, synsem:(loc:cat:(head:(verb,
                                   vform:fin),
                             val:(Val,(subj:e_list,
                                     comps:e_list))),
                    nonloc:(to_bind:[Local],
                            inherited:List)),
       phon:HdtrPhon),
goal> (our_member(Local,List), append(NdtrPhon,HdtrPhon,MotherPhon)).


% functional definition of our-member for the Subject Condition

fun our_member(+,-).

our_member(Hd,[Hd|_]) if true.
our_member(Hd,[_|Tl]) if our_member(Hd,Tl).


%====================================================================
% PRINCIPLES

% Head Feature Principle

phrase *> (synsem:loc:cat:head:Head,
           daughters:hdtr:synsem:loc:cat:head:Head).

% Semantics Principle

phrase *> ((synsem:loc:cont:Content,
	daughters:((hs_struc;hc_struc;hf_struc;sai_struc),
	         hdtr:synsem:loc:cont:Content);
            (synsem:loc:cont:Content,
	 daughters:(ha_struc,
                      ndtr:synsem:loc:cont:Content)))).

% INV Principle

(synsem:loc:cat:head:inv:plus) *> (synsem:loc:cat:head:(vform:fin,
                                                        aux:plus)).

% MOD Principle

(phrase,
 daughters:(hs_struc;hc_struc;sai_struc;hf_struc))
  *> daughters:ndtr:synsem:loc:cat:head:mod:none.


% Argument Realization Principle

(word,
 synsem:loc:cat:head:pred:plus) *> (synsem:loc:cat:val:(subj:[Synsem],
                                                        comps:List),
                                    arg_st:[Synsem|List]).

(word,
 synsem:loc:cat:head:pred:minus) *> (synsem:loc:cat:val:(subj:e_list,
                                                        comps:List),
                                    arg_st:List).


% Structural Case Principle

Synsem^(phrase,
 daughters:(hdtr:synsem:loc:cat:(head:vform:fin,
                                 val:subj:[Synsem]),
            ndtr:synsem:(Synsem,
                         loc:cat:head:noun)))
*> (daughters:ndtr:synsem:loc:cat:head:case:nom).

(phrase,
 daughters:(hc_struc,
            ndtr:synsem:loc:cat:head:noun))
*> (daughters:ndtr:synsem:loc:cat:head:case:acc).


% Functional Preposition Principle

(word,
phon:ne_list,
synsem:loc:cat:head:(prep,
              pform:non_lexical))
*>
(synsem:loc:(cat:(head:(mod:none,pred:minus)),
              cont:Cont),
arg_st:([(loc:(cat:val:(subj:[],comps:[]),
               cont:Cont))])
).


% Subject Principles

(val,subj:ne_list) *> subj:[_].

(head:verb,val:subj:e_list) *> head:vform:fin.


% The Nonlocal Feature Principle

phrase *>
   (synsem:nonloc:inherited:Result,
 daughters:(hdtr:synsem:nonloc:(inherited:List1,
                               to_bind:Subtrac),
	  ndtr:synsem:nonloc:inherited:List2))
goal
   nfp(List1,List2,Subtrac,Result).

nfp(List1,List2,[],Result) if append(List1,List2,Result).
nfp(L1,L2,([Ele]),Result) if (append(L1,L2,L3),select(Ele,L3,Result)).

append(X,Y,Z) if
   when(  ( X=(e_list;ne_list)
          ; Y=e_list
          ; Z=(e_list;ne_list)
                ),
            undelayed_append(X,Y,Z)).

undelayed_append(L,[],L) if true.
undelayed_append([],(L,ne_list),L) if true.
undelayed_append([H|T1],(L,ne_list),[H|T2]) if
  append(T1,L,T2).

select(Ele,L1,L2) if
   when(  ( L1=(e_list;ne_list)
                ),
            und_select(Ele,L1,L2)).

und_select(Ele,[Ele|Tl],Tl) if true.
und_select(Ele,[Hd|Tl1],[Hd|Tl2]) if select(Ele,Tl1,Tl2).


% Phrasal to-bind Principle

(phrase,
 daughters:hdtr:(phrase,
                 synsem:nonloc:to_bind:ne_list))
*> (daughters:hf_struc).

(phrase,daughters:(hs_struc;hc_struc;sai_struc;ha_struc))
 *> daughters:hdtr:synsem:nonloc:to_bind:e_list.


% Structural Trace Principle

% Clause (1)

(daughters:ndtr:phon:e_list) *>
 (synsem:(nonloc:to_bind:e_list,               % no vacuous movement
         loc:cat:head:(verb;noun;prep;adv)),   % not functional
  daughters: (hs_struc;hc_struc;sai_struc)). % follows already independently
                                             % from the implementation

% Clause (2) and (3)
% The effect clauses (2) and (3) logically follows from the more specific
% lexical entry of traces in the implementation (see below): This grammar
% only contains nominal and prepositional traces. Moreover, they are
% syntactically saturated. This is equivalent to restricting a more
% general entry of a trace by a principle and to forbidding the extraction
% of head daughters.


% Subject Condition

(word,
 synsem:loc:cat:head:pred:plus,
 arg_st:hd:nonloc:inherited:ne_list) *>
( (arg_st:[(loc:Loc,
	    nonloc:inherited:[Loc])|_]);
  (arg_st:[_|our_member((nonloc:inherited:ne_list))])).



%====================================================================
% LEXICON

% NOUNS

peter ---> (synsem:(loc:(cat:head:(noun,
			    pred:minus),
		    cont:index:(ref,
                                num:sg,
		                pers:third,
                                gen:masc)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

mary ---> (synsem:(loc:(cat:head:(noun,
			    pred:minus),
		    cont:index:(ref,
                                num:sg,
		                pers:third,
                                gen:fem)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

he ---> (synsem:(loc:(cat:head:(noun,
			  case:nom,
			  pred:minus),
		    cont:index:(ref,
                                num:sg,
		                pers:third,
                                gen:masc)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

she ---> (synsem:(loc:(cat:head:(noun,
			  case:nom,
			  pred:minus),
		    cont:index:(ref,
                                num:sg,
		                pers:third,
                                    gen:fem)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

it ---> (synsem:(loc:(cat:head:(noun,
			    pred:minus),
		    cont:index:(ref,
                                num:sg,
		                pers:third,
                                gen:neut)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

it ---> (synsem:(loc:(cat:head:(noun,
			    pred:minus),
		    cont:index:(it,
                                num:sg,
		                pers:third,
                                gen:neut)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

him ---> (synsem:(loc:(cat:head:(noun,
			  case:acc,
			  pred:minus),
		    cont:index:(ref,
                                num:sg,
		                pers:third,
                                gen:masc)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

her ---> (synsem:(loc:(cat:head:(noun,
			  case:acc,
			  pred:minus),
		    cont:index:(ref,
                                num:sg,
		                pers:third,
                                gen:fem)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

you ---> (synsem:(loc:(cat:head:(noun,
			  pred:minus),
		    cont:index:(ref,
                               pers:second)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

they ---> (synsem:(loc:(cat:head:(noun,
			  case:nom,
			  pred:minus),
		    cont:index:(ref,
                               pers:third,
                               num:pl)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

them ---> (synsem:(loc:(cat:head:(noun,
			        case:acc,
			        pred:minus),
		    cont:index:(ref,
                               pers:third,
                               num:pl)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:e_list).

empty  word,
       phon:e_list,
       synsem:(loc:(Local,
                  cat:head:(noun,
		            pred:minus)),
              nonloc:(inherited:[Local],
                      to_bind:e_list)),
       arg_st:e_list.           % the specification arg_st:e_list excludes
                                % heads in nominal phrases from being
                                % extracted because it entails that the
                                % extracted element is fully saturated


% VERBS

walk --->  (synsem:(loc:(cat:head:(verb,
			         vform: base,
                                 pred: plus,
                                 aux: minus),
		    cont:(walk_rel,
		        walker:Index)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:[(loc:(cat:(head:noun,
		         val:(subj:e_list,
                                  comps:e_list)),
                          cont:index:Index))]).


rain --->  (synsem:(loc:(cat:head:(verb,
			         vform: base,
                                  pred: plus,
                                 aux: minus),
		    cont:rain_rel),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:[(loc:(cat:(head:noun,
		                  val:(subj:e_list,
                                  comps:e_list)),
                          cont:index:it))]).

like --->  (synsem:(loc:(cat:head:(verb,
			         vform: base,
                                 pred: plus,
                                 aux: minus),
		    cont:(like_rel,
		          liker:Index1,
                              liked:Index2)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:[(loc:(cat:(head:noun,
		         val:(subj:e_list,
                                  comps:e_list)),
                          cont:index:Index1)),
                     (loc:(cat:(head:noun,
		         val:(subj:e_list,
                                  comps:e_list)),
                          cont:index:Index2))]).


say --->  (synsem:(loc:(cat:head:(verb,
			          vform: base,
                                   pred: plus,
                                  aux: minus),
		    cont:(say_rel,
		          sayer:Index,
                          said:Psoa)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:[(loc:(cat:(head:noun,
		         val:(subj:e_list,
                                  comps:e_list)),
                          cont:index:Index)),
                     (loc:(cat:(head:(functional,
                                    vform:fin,
				    marking:that),
		         val:(subj:e_list,
                                  comps:e_list)),
                          cont:Psoa))]).


give --->  (synsem:(loc:(cat:head:(verb,
			         vform: base,
                                 pred: plus,
                                 aux: minus),
		    cont:(give_rel,
		          giver:Index1,
                              gift:Index2,
                              given:Index3)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
            arg_st:[(loc:(cat:(head:noun,
		         val:(subj:e_list,
                                  comps:e_list)),
                          cont:index:Index1)),
                     (loc:(cat:(head:noun,
		         val:(subj:e_list,
                                  comps:e_list)),
                          cont:index:Index2)),
                     (loc:(cat:(head:(prep,
                                      pform:to),
		         val:(subj:e_list,
                                  comps:e_list)),
                          cont:index:Index3))]).


%%% AUXILIARIES

% LE of is (passive auxiliary)

be ---> (synsem:(loc:(cat:head:(verb,
		             vform:base,
                             pred:plus,
                             aux:plus),
                     cont:Cont),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
      arg_st:[(Synsem),(loc:(cat:(head:(verb,
                                        vform:pas),
                                  val:(subj:[(Synsem)],
                                       comps:e_list)),
		       cont:Cont))]).

% LE of will (future auxiliary)

will ---> (synsem:(loc:(cat:head:(verb,
		             vform:fin,
                             pred:plus,
                             aux:plus),
                     cont:(future_rel,
                          soa_arg:Cont)),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
      arg_st:[(Synsem),(loc:(cat:(head:(verb,
                                        vform:base),
                                  val:(subj:[(Synsem)],
                                       comps:e_list)),
		              cont:Cont))]).


have ---> (synsem:(loc:(cat:head:(verb,
		             vform:base,
                             pred:plus,
                             aux:plus),
                     cont:Cont),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
      arg_st:[(Synsem),(loc:(cat:(head:(verb,
                                        vform:psp),
                                  val:(subj:[(Synsem)],
                                       comps:e_list)),
		       cont:Cont))]).

be ---> (synsem:(loc:(cat:head:(verb,
		             vform:base,
                             pred:plus,
                             aux:plus),
                     cont:Cont),
	         nonloc:(inherited:e_list,
		        to_bind:e_list)),
      arg_st:[(Synsem),(loc:(cat:(head:(verb,
                                        vform:prp),
                                  val:(subj:[(Synsem)],
                                       comps:e_list)),
		       cont:Cont))]).



% PREPOSITIONS

by ---> (word, synsem:(loc:cat:head:(prep,
                                   pform:by),
                      nonloc:(inherited:[],
                              to_bind:[]))).

to ---> (word, synsem:(loc:cat:head:(prep,
                                   pform:to),
                      nonloc:(inherited:[],
                              to_bind:[]))).

to ---> (synsem:(loc:(cat:head:(prep,
	                       mod:(loc:(cat:(head:verb,
                                              val:(subj:ne_list,
                                                   comps:e_list)),
                                         cont:(Cont1))),
		               pred:minus,
                               pform:lexical),
		      cont:search_rel(Cont2,Cont1)),
	        nonloc:(inherited:[],
                        to_bind:[])),
        arg_st:[(loc:(cat:(head:noun,
                           val:(subj:e_list,
                                comps:e_list)),
                      cont:index:Cont2))]).

fun search_rel(+,+,-).

search_rel(C,X,Y) if
  when( (X=(move_rel;tense_rel;like_rel;say_rel;give_rel;rain_rel;
            direction_rel;here_rel) ),
           (und_search_rel(C,(X,(move_rel;tense_rel)),Y);fail)).

und_search_rel(C,X,Y) if (X=move_rel),(Y=(direction_rel,
                                          movement:X,
                                          goal:C)).
und_search_rel(C,X,Y) if (X=(future_rel,
                             soa_arg:Cont)),(Y=(future_rel,
                                               soa_arg:search_rel(C,Cont))).
und_search_rel(C,X,Y) if (X=(perfect_rel,
                             soa_arg:Cont)),(Y=(perfect_rel,
                                               soa_arg:search_rel(C,Cont))).
und_search_rel(C,X,Y) if (X=(present_rel,
                             soa_arg:Cont)),(Y=(present_rel,
                                               soa_arg:search_rel(C,Cont))).
und_search_rel(C,X,Y) if (X=(past_rel,
                             soa_arg:Cont)),(Y=(past_rel,
                                               soa_arg:search_rel(C,Cont))).
und_search_rel(C,X,Y) if (X=(cont_rel,
                             soa_arg:Cont)),(Y=(cont_rel,
                                               soa_arg:search_rel(C,Cont))).

empty  word,
       phon:e_list,
       synsem:(loc:(Local,
                  cat:head:(prep,
			    mod:none,
		            pred:minus,
                            pform:(to;by))),  % restricted to the 2 functional
              nonloc:(inherited:[Local],      % prepositions in the fragment
                      to_bind:e_list)),
       arg_st:e_list.           % the specification arg_st:e_list excludes
                                % heads in prepositional phrases from being
                                % extracted because it entails that the
                                % extracted element is fully saturated


% ADVERBS

here ---> (synsem:(loc:(cat:head:(adv,
		         pred:minus,
                             mod:loc:(cat:(head:verb,
				 val:subj:ne_list),
                                      cont:Cont)),
                     cont:(here_rel,
                          located:Cont)),
	            nonloc:(inherited:e_list,
                           to_bind:e_list)),
        arg_st:[]).


% COMPLEMENTIZERS

that ---> (synsem:(loc:(cat:head:(functional,
			         pred:minus,
			         vform:Vform,
			 mod:none,
                                 marking:that),
                       cont:Cont),
                  nonloc:(inherited:e_list,
                          to_bind:e_list)),
          arg_st:[(loc:(cat:(head:(verb,
				 vform:(Vform,fin)),
			   val:(subj:e_list,
                                comps:e_list)),
		      cont:Cont))]).


%====================================================================

phonology
forall Word ---> FS do
 FS = phon:[(a_ Word)].



%====================================================================
% Lexical Rules

% PSP Lexical Rule

psp_lex_rule lex_rule
(word,
 synsem:(loc:(cat:head:(vform:base,
		       aux:Aux,
                       pred:Pred),
	      cont:Cont),
         nonloc:Nonloc),
 arg_st:Arg)
 **>
(synsem:(loc:(cat:head:(vform:psp,
		       aux:Aux,
                       pred:Pred),
             cont:(perfect_rel,
                   soa_arg:Cont)),
         nonloc:Nonloc),
 arg_st:Arg)
morphs
be becomes been,
give becomes given,
have becomes had,
say becomes said,
(X,[e]) becomes (X,ed),
X becomes (X,ed).

% Present Participle Lexical Rule

prp_lex_rule lex_rule
(word,
 synsem:(loc:(cat:head:(vform:base,
		       aux:Aux,
                       pred:Pred),
	     cont:Cont),
         nonloc:Nonloc),
 arg_st:Arg)
 **>
(synsem:(loc:(cat:head:(vform:prp,
		       aux:Aux,
                       pred:Pred),
             cont:(cont_rel,
                   soa_arg:Cont)),
         nonloc:Nonloc),
 arg_st:Arg)
morphs
be becomes being,
(X,[e]) becomes (X,ing),
X becomes (X,ing).


% Passive Lexical Rule

passive_lex_rule lex_rule
(word,
 synsem:(loc:(cat:head:(verb,
  		    vform:psp,
                    aux:(Aux,minus),      % no passive of auxiliaries
                    inv:Inv,
                    pred:Pred),
	  cont:soa_arg:Cont),
       nonloc:Nonloc),
 arg_st:[(loc:(cat:(head:noun,
                    val:(subj:e_list,
                         comps:e_list)),
              cont:Cont2)),
         Synsem1|
         List])
 **>
(word,
 synsem:(loc:(cat:head:(verb,
		    vform:pas,
                    aux:Aux,
                    inv:Inv,
                    pred:Pred),
	  cont:Cont),
         nonloc:Nonloc),
 arg_st:([Synsem1|List];Result))
if append(([Synsem1|List]),[(loc:(cat:(head:(prep,
                                       pform:by),
                                      val:(subj:e_list,
                                           comps:e_list)),
                                 cont:Cont2))],Result)
morphs
X becomes X.


% 3rd_sing_fin_lex_rule

third_sing_fin_lex_rule lex_rule
(word,
 synsem:(loc:(cat:head:(vform:base,
		       aux:Aux,
                       pred:Pred),
	     cont:Cont),
         nonloc:Nonloc),
 arg_st:Arg)
 **>
(synsem:(loc:(cat:(head:(vform:fin,
		       aux:Aux,
                       pred:Pred),
		  val:subj:[(loc:cont:(psoa;index:(pers:third,
                                                   num:sg)))]),
             cont:(present_rel,
                  soa_arg:Cont)),
         nonloc:Nonloc),
 arg_st:Arg)
morphs
be becomes is,
have becomes has,
X becomes (X,s).

% non_3rd_sing_fin_lex_rule

non_third_sing_fin_lex_rule lex_rule
(word,
 synsem:(loc:(cat:head:(vform:base,
		       aux:Aux,
                       pred:Pred),
	     cont:Cont),
         nonloc:Nonloc),
 arg_st:Arg)
 **>
(synsem:(loc:(cat:(head:(vform:fin,
		       aux:Aux,
                       pred:Pred),
		  val:subj:[(loc:cont:index:(num:pl;
                                             (pers:(first;second),
                                             num:sg)))]),
             cont:(present_rel,
                   soa_arg:Cont)),
         nonloc:Nonloc),
 arg_st:Arg)
morphs
be becomes are,
X becomes X.


% past_lex_rule no1

non_third__first_sing_past_lex_rule lex_rule
(word,
 synsem:(loc:(cat:head:(vform:base,
		       aux:Aux,
                       pred:Pred),
	     cont:Cont),
         nonloc:Nonloc),
 arg_st:Arg)
 **>
(synsem:(loc:(cat:(head:(vform:fin,
		       aux:Aux,
                       pred:Pred),
		  val:subj:[(loc:cont:index:(num:pl;
                                             (pers:second,
                                             num:sg)))]),
             cont:(past_rel,
                   soa_arg:Cont)),
         nonloc:Nonloc),
 arg_st:Arg)
morphs
be becomes were,
give becomes gave,
have becomes had,
say becomes said,
(X,[e]) becomes (X,ed),
X becomes (X,ed).


% past_lex_rule no2

third__first_sing_past_lex_rule lex_rule
(word,
 synsem:(loc:(cat:head:(vform:base,
		       aux:Aux,
                       pred:Pred),
	     cont:Cont),
         nonloc:Nonloc),
 arg_st:Arg)
 **>
(synsem:(loc:(cat:(head:(vform:fin,
		       aux:Aux,
                       pred:Pred),
		  val:subj:[(loc:cont:(psoa;index:(pers:(first;third),
                                                   num:sg)))]),
             cont:(past_rel,
                   soa_arg:Cont)),
         nonloc:Nonloc),
 arg_st:Arg)
morphs
be becomes was,
give becomes gave,
have becomes had,
say becomes said,
(X,[e]) becomes (X,ed),
X becomes (X,ed).


%====================================================================




