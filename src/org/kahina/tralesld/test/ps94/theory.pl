% Multifile declarations.
:- multifile if/2.

% specify signature file
signature(signature).

% grisu output specifications

hidden_feat(dtrs).         % shown by the tree
hidden_feat(daughters).    % shown by the tree

synsem <<< arg_st.
arg_st <<< qstore.
qstore <<< retr.
quants <<< nucleus.
vform <<< aux.
vform <<< inv.
liker <<< liked.
sayer <<< said.
giver <<< gift.
gift <<< given.
>>> phon.


%====================================================================


% Definite Clauses

collect_phons([],[]) if true.
collect_phons([(phon:Phon)|Tlsigns],ListofPhons) if
                                        collect_phons(Tlsigns,Tlphons),
					append(Phon,Tlphons,ListofPhons).

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



%====================================================================
% Phrase Structure Rules, encoding the ID-Principle and the
%                                      Constituent Order Principle

% Schema 1: Head Subject Rule

head_subject_rule rule
(phrase,
 phon:MotherPhon,
 synsem:(loc:cat:(head:((inv:minus);(functional;non_verb_subst)),
		  subcat:[])),
 daughters:(head_comp_struc,
            c_dtrs:[(Cdtr)],
            hdtr:(phrase, Hdtr)),
 dtrs:[Cdtr,Hdtr])
   ===>
cat> (Cdtr, phon:CPhon),
cat> (Hdtr, phon:HdPhon),
goal> append(CPhon,HdPhon,MotherPhon).


% Schema 2: Head Complement Rule

head_complement_rule rule
(phrase,
 phon:MotherPhon,
 synsem:(loc:cat:(head:((inv:minus);(functional;non_verb_subst)),
		  subcat:[_])),
 daughters:(head_comp_struc,
            hdtr:(word, Hdtr),
            c_dtrs:(Ndtrs)),
 dtrs:[Hdtr|Ndtrs])
   ===>
cat> (Hdtr, phon:HdPhon),
cats> (([];[_];[_,_]), Ndtrs),  % ,Ndtrs)),
goal> (collect_phons(Ndtrs,(CompsPhon)),
       append(HdPhon,CompsPhon,MotherPhon)).

% Schema 2': Prep-Head Complement Rule (fixing a mistake by P&S:94, who
%                                       exclude this construction)

head_complement_rule2 rule
(phrase,
 phon:MotherPhon,
 synsem:(loc:cat:(head:prep,
		  subcat:[])),
 daughters:(head_comp_struc,
            hdtr:(word, Hdtr),
            c_dtrs:([Cdtr], Ndtrs)),
 dtrs:[Hdtr|Ndtrs])
   ===>
cat> (Hdtr, phon:HdPhon),
cats> ([Cdtr], Ndtrs),
goal> (collect_phons(Ndtrs,(CompsPhon)),
       append(HdPhon,CompsPhon,MotherPhon)).


% Schema 4: Head Marker Rule

head_marker_rule rule
(phrase,
 phon:MotherPhon,
 synsem:loc:cat:subcat:[],
 daughters:(head_mark_struc,
            marker_dtr:(Mdtr, synsem:loc:cat:head:mark),
            hdtr:(phrase, Hdtr)),
 dtrs:[Mdtr,Hdtr])
   ===>
cat> (Mdtr, phon:MPhon),
cat> (Hdtr, phon:HdPhon),
goal> append(MPhon,HdPhon,MotherPhon).


%====================================================================
% PRINCIPLES

% Head Feature Principle

phrase *> (synsem:loc:cat:head:Head,
           daughters:hdtr:synsem:loc:cat:head:Head).


% Subcategorization Principle

(phrase,
 daughters:headed_struc) *>
(synsem:loc:cat:subcat:MotherSubcat,
 daughters:(hdtr:synsem:loc:cat:subcat:HdSubcat,
	    c_dtrs:CdtrsList))
goal sign_ss_append(MotherSubcat,CdtrsList,HdSubcat).

sign_ss_append(X,Y,Z) if
   when(  ( X=(e_list;ne_list)
          , Y=(e_list;ne_list)
          , Z=(e_list;ne_list)
                ),
            undel_sign_ss_append(X,Y,Z)).

undel_sign_ss_append(L,[],L) if true.
undel_sign_ss_append([],[(synsem:First)|Tl1],[First|Tl2]) if
                                    sign_ss_append([],Tl1,Tl2).
undel_sign_ss_append([Hd|T1],(L,ne_list),[Hd|T2]) if
                                    sign_ss_append(T1,L,T2).


% Marking Principle

(phrase,
 daughters:head_mark_struc) *>
(synsem:loc:cat:marking:Marking,
 daughters:marker_dtr:synsem:loc:cat:marking:Marking).

(phrase,
 daughters:(head_comp_struc;head_adj_struc)) *>
 (synsem:loc:cat:marking:Marking,
  daughters:hdtr:synsem:loc:cat:marking:Marking).


% Spec Principle

(phrase,
 daughters:head_mark_struc) *>
(daughters:(marker_dtr:synsem:loc:cat:head:spec:Synsem,
	    hdtr:synsem:Synsem)).

(phrase,
 daughters:(head_comp_struc,
	    c_dtrs:[(synsem:loc:cat:head:functional)])) *>
(daughters:(c_dtrs:[(synsem:loc:cat:head:spec:Synsem)],
	    hdtr:synsem:Synsem)). 


% Semantics Principle

% Clause a

% Clause a

% Part I
% (head-adjunct structures and head-marker structures)

(phrase,
 daughters:(head_adj_struc;head_mark_struc,
	    hdtr:((word;phrase),
		  qstore:(e_list;ne_list),
		  retr:(e_list;ne_list)))) *>
(qstore:Qstore,
 retr:Retr,
 daughters:Const_struc)
goal (collect_dtrs_qstores(Const_struc,CollectedQstore),
      qstore_retrieval(Retr,Qstore,CollectedQstore)).


% Part II
% (head-complement structures)

(phrase,
 daughters:(head_comp_struc,
	    hdtr:((word;phrase),
		  qstore:(e_list;ne_list),
		  retr:(e_list;ne_list)),
	    c_dtrs:([];
	            hd:((word;phrase),
		       qstore:(e_list;(hd:quant)),
		       retr:(e_list;(hd:quant)))))) *>
(qstore:Qstore,
 retr:Retr,
 daughters:(Const_struc,
	    c_dtrs:Cdtrs))
goal (collect_dtrs_qstores(Const_struc,CollectedQstore),
      qstore_retrieval(Retr,Qstore,CollectedQstore)).


qstore_retrieval([],[],[]) if true.
qstore_retrieval([],(List,ne_list),List) if true.
qstore_retrieval((R,ne_list),Q,(List,ne_list)) if
                                length_limit(List), % set to max. 3 quantifiers
                                select_any_number_order(List,R,Q).

length_limit(X) if  X=([_];[_,_];[_,_,_]).
% can be increased if more than 3 quantifiers are allowed to co-occur

select_any_number_order(Input,[],Input) if true.
select_any_number_order(Input,[Ele|SInput],Restlist) if
                              select_one(Ele,Input,Rest),
		              select_any_number_order(Rest,SInput,Restlist).


select_one(Ele,[Ele|Rest],Rest) if true.
select_one(Ele,[Hd|Tail],[Hd|Rest]) if select_one(Ele,Tail,Rest).


% Clause b, Conj 1, Disj 1

(phrase,
 daughters:(head_adj_struc,
	    adj_dtr:synsem:loc:cont:psoa)) *>
(synsem:loc:cont:(quants:QuantsM,
		  nucleus:Nucleus),
 retr:(Retr, []),                  %% set to elist: no retrieval possible
 daughters:adj_dtr:synsem:loc:cont:(quants:QuantsD,
		        	    nucleus:Nucleus))
goal append(Retr,QuantsD,QuantsM).


% Clause b, Conj 1, Disj 2

(phrase,
 daughters:(non_head_adj_struc,
	    hdtr:synsem:loc:cont:psoa)) *>
(synsem:loc:cont:(quants:QuantsM,
		  nucleus:Nucleus),
 retr:(Retr, []),                  %% set to elist: no retrieval possible
 daughters:hdtr:synsem:loc:cont:(quants:QuantsD,
				 nucleus:Nucleus))
goal append(Retr,QuantsD,QuantsM).


% Clause b, Conj 2, Disj 1

(phrase,
 daughters:(head_adj_struc,
	    adj_dtr:synsem:loc:cont:non_psoa)) *>
(synsem:loc:cont:Cont,
 retr:[],
 daughters:adj_dtr:synsem:loc:cont:Cont).

% Clause b, Conj 2, Disj 2

(phrase,
 daughters:(non_head_adj_struc,
	    hdtr:synsem:loc:cont:non_psoa)) *>
(synsem:loc:cont:Cont,
 retr:[],
 daughters:hdtr:synsem:loc:cont:Cont).



% INV Principle

(synsem:loc:cat:head:inv:plus) *> (synsem:loc:cat:head:(vform:fin,
                                                        aux:plus)).

% Argument Realization Principle

word *>
(synsem:loc:cat:subcat:List,
 arg_st:List).


% Functional Preposition Principle

(word,
phon:ne_list,
synsem:loc:cat:head:(prep,
                     pform:non_lexical))
*>
(synsem:loc:(cat:(head:(mod:none,pred:minus)),
             cont:Cont),
 qstore:[],
 retr:[],
arg_st:([(loc:(cat:subcat:[],
               cont:Cont))])
).


% Subject Principle

(head:verb,subcat:e_list) *> head:vform:fin.


% Lexical Marking Value Principle

(word,
 synsem:loc:cat:head:(subst;det)) *>
 synsem:loc:cat:marking:unmarked.

%====================================================================
% LEXICON

% NOUNS

peter ---> (word,
            phon:[(a_ peter)],
	    synsem:loc:(cat:head:(noun,
                                  pred:minus),
                        cont:(npro,
			      index:(ref,
                                    num:sg,
                                    pers:third,
                                    gen:masc),
			      restr:[])),
	    qstore:[],
	    retr:[],
            arg_st:e_list).

mary ---> (word,
           phon:[(a_ mary)],
	   synsem:loc:(cat:head:(noun,
                                  pred:minus),
                      cont:(npro,
		            index:(ref,
                                  num:sg,
                                  pers:third,
                                  gen:fem),
			    restr:[])),
	   qstore:[],
	   retr:[],
           arg_st:e_list).

he ---> (word,
         phon:[(a_ he)],
	 synsem:loc:(cat:head:(noun,
                              case:nom,
                              pred:minus),
                     cont:(pron,
		           index:(ref,
                                 num:sg,
                                 pers:third,
                                 gen:masc),
			   restr:[])),
	qstore:[],
	retr:[],
        arg_st:e_list).

she ---> (word,
          phon:[(a_ she)],
	  synsem:loc:(cat:head:(noun,
                                case:nom,
                                pred:minus),
                     cont:(pron,
		           index:(ref,
                                 num:sg,
                                 pers:third,
                                 gen:fem),
			   restr:[])),
	qstore:[],
	retr:[],
        arg_st:e_list).

it ---> (word,
         phon:[(a_ it)],
	 synsem:loc:(cat:head:(noun,
                                  pred:minus),
                    cont:(pron,
		          index:(ref,
                                num:sg,
                                pers:third,
                                gen:neut),
			  restr:[])),
	qstore:[],
	retr:[],
        arg_st:e_list).

it ---> (word,
         phon:[(a_ it)],
	 synsem:loc:(cat:head:(noun,
                              pred:minus),
                    cont:(pron,
		          index:(it,
                                num:sg,
                                pers:third,
                                gen:neut),
			  restr:[])),
	 qstore:[],
	 retr:[],
         arg_st:e_list).

him ---> (word,
          phon:[(a_ him)],
	  synsem:loc:(cat:head:(noun,
                               case:acc,
                               pred:minus),
                     cont:(pron,
		           index:(ref,
                                num:sg,
                                pers:third,
                                gen:masc),
			   restr:[])),
	  qstore:[],
	  retr:[],
          arg_st:e_list).

her ---> (word,
          phon:[(a_ her)],
	  synsem:loc:(cat:head:(noun,
                               case:acc,
                               pred:minus),
                     cont:(pron,
		           index:(ref,
                                num:sg,
                                pers:third,
                                gen:fem),
			   restr:[])),
	  qstore:[],
	  retr:[],
          arg_st:e_list).

you ---> (word,
          phon:[(a_ you)],
	  synsem:loc:(cat:head:(noun,
                               pred:minus),
                     cont:(pron,
		           index:(ref,
                                 pers:second),
			   restr:[])),
	  qstore:[],
	  retr:[],
          arg_st:e_list).

they ---> (word,
           phon:[(a_ they)],
	   synsem:loc:(cat:head:(noun,
                                case:nom,
                                pred:minus),
                       cont:(pron,
		             index:(ref,
                                   pers:third,
                                   num:pl),
			     restr:[])),
	   qstore:[],
	   retr:[],
           arg_st:e_list).

them ---> (word,
           phon:[(a_ them)],
	   synsem:loc:(cat:head:(noun,
                                case:acc,
                                pred:minus),
                       cont:(pron,
		             index:(ref,
                                   pers:third,
                                   num:pl),
			     restr:[])),
	    qstore:[],
	    retr:[],
            arg_st:e_list).


% Count Nouns

poem   --->  (word,
              phon:[(a_ poem)],
	      synsem:loc:(cat:head:(noun,
                                    pred:minus),
                          cont:(npro,
			        index:(Index,
				       ref,
                                       num:sg,
                                       pers:third,
				       gen:neut),
				restr:[(poem_rel,
					inst:Index)])),
              qstore:[],
	      retr:[],
              arg_st:[(loc:(cat:(head:det,
			         subcat:[])))]).

student ---> (word,
              phon:[(a_ student)],
	      synsem:loc:(cat:head:(noun,
                                    pred:minus),
                          cont:(npro,
			        index:(Index,
				       ref,
                                       num:sg,
                                       pers:third),
				restr:[(student_rel,
					inst:Index)])),
	      qstore:[],
	      retr:[],
              arg_st:[(loc:(cat:(head:det,
			         subcat:[])))]).

boy --->     (word,
              phon:[(a_ boy)],
	      synsem:loc:(cat:head:(noun,
                                    pred:minus),
                          cont:(npro,
			        index:(Index,
				       ref,
				       gen:masc,
                                       num:sg,
                                       pers:third),
				restr:[(boy_rel,
					inst:Index)])),
	      qstore:[],
	      retr:[],
              arg_st:[(loc:(cat:(head:det,
			         subcat:[])))]).


% DETERMINERS

a  --->    (word,
	    phon:[(a_ a)],
	    synsem:loc:(cat:(head:(det,
				 spec:loc:(cat:(head:noun,
					       subcat:[_]
					       ),
					  cont:NounCont)
				 ),
			    marking:unmarked),
			cont:(Quant,
			      quant,
			      det:exists,
			      restind:NounCont)),
             qstore:[(Quant)],
	     retr:[],
	     arg_st:[]).


every ---> (word,
	    phon:[(a_ every)],
	    synsem:loc:(cat:(head:(det,
				 spec:loc:(cat:(head:noun,
					       subcat:[_]
					       ),
					  cont:NounCont)
				 ),
			    marking:unmarked),
			cont:(Quant,
			      quant,
			      det:forall,
			      restind:NounCont)),
             qstore:[(Quant)],
	     retr:[],
	     arg_st:[]).


one   ---> (word,
	    phon:[(a_ one)],
	    synsem:loc:(cat:(head:(det,
				 spec:loc:(cat:(head:noun,
					       subcat:[_]
					       ),
					  cont:NounCont)
				 ),
			    marking:unmarked),
			cont:(Quant,
			      quant,
			      det:one,
			      restind:NounCont)),
             qstore:[(Quant)],
	     retr:[],
	     arg_st:[]).


% VERBS

walk --->  (word,
            phon:[(a_ walk)],
	    synsem:loc:(cat:head:(verb,
                                 vform: base,
                                 pred: plus,
                                 aux: minus),
                        cont:(quants:[],
			      nucleus:(walk_rel,
                                      walker:Index))),
	    qstore:[],
	    retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:Index))]).

walks --->  (word,
            phon:[(a_ walks)],
	    synsem:loc:(cat:(head:(verb,
                                 vform:fin,
                                 pred:plus,
                                 aux:minus),
                             subcat:[(loc:cont:(psoa;index:(pers:third,
                                                            num:sg)))]),
                        cont:(quants:[],
			      nucleus:(walk_rel,
                                      walker:Index))),
            qstore:[],
            retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:Index))]).


rain --->  (word,
            phon:[(a_ rain)],
	    synsem:loc:(cat:head:(verb,
                                 vform: base,
                                 pred: plus,
                                 aux: minus),
                        cont:(quants:[],
			      nucleus:rain_rel)),
	    qstore:[],
	    retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:it))]).

rains --->  (word,
            phon:[(a_ rains)],
	    synsem:loc:(cat:(head:(verb,
                                  vform:fin,
                                  pred:plus,
                                  aux:minus),
                             subcat:[(loc:cont:(psoa;index:(pers:third,
                                                              num:sg)))]),
                        cont:(quants:[],
			      nucleus:rain_rel)),
            qstore:[],
            retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:it))]).

like --->  (word,
            phon:[(a_ like)],
	    synsem:loc:(cat:head:(verb,
                                 vform: base,
                                 pred: plus,
                                 aux: minus),
                        cont:(quants:[],
			      nucleus:(like_rel,
                                      liker:Index1,
                                      liked:Index2))),
	    qstore:[],
	    retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:Index1)),
                     (loc:(cat:(head:noun,
                                subcat:[]),
                          cont:index:Index2))]).

likes --->  (word,
             phon:[(a_ likes)],
	     synsem:loc:(cat:(head:(verb,
                                   vform: fin,
                                   pred: plus,
                                   aux: minus),
                              subcat:[(loc:cont:(psoa;index:(pers:third,
                                                              num:sg)))|_]),
                        cont:(quants:[],
			      nucleus:(like_rel,
                                      liker:Index1,
                                      liked:Index2))),
	    qstore:[],
	    retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:Index1)),
                     (loc:(cat:(head:noun,
                                subcat:[]),
                          cont:index:Index2))]).


say --->  (word,
           phon:[(a_ say)],
	   synsem:loc:(cat:head:(verb,
                                vform: base,
                                pred: plus,
                                aux: minus),
                       cont:(quants:[],
		             nucleus:(say_rel,
                                     sayer:Index,
                                     said:Psoa))),
	    qstore:[],
	    retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:Index)),
                     (loc:(cat:(head:vform:fin,
                                marking:that,
                                subcat:[]),
                            cont:Psoa))]).

says --->  (word,
            phon:[(a_ says)],
	    synsem:loc:(cat:(head:(verb,
                                  vform: fin,
                                  pred: plus,
                                  aux: minus),
                             subcat:[(loc:cont:(psoa;index:(pers:third,
                                                            num:sg)))|_]),
                        cont:(quants:[],
			      nucleus:(say_rel,
                                      sayer:Index,
                                      said:Psoa))),
	    qstore:[],
	    retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:Index)),
                     (loc:(cat:(head:vform:fin,
                                marking:that,
                                subcat:[]),
                           cont:Psoa))]).

give --->  (word,
            phon:[(a_ give)],
	    synsem:loc:(cat:head:(verb,
                                 vform: base,
                                 pred: plus,
                                 aux: minus),
                        cont:(quants:[],
			      nucleus:(give_rel,
                                      giver:Index1,
                                      gift:Index2,
                                      given:Index3))),
	    qstore:[],
	    retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:Index1)),
                     (loc:(cat:(head:noun,
                                subcat:[]),
                           cont:index:Index2)),
                     (loc:(cat:(head:(prep,
                                      pform:to),
                                subcat:[]),
                           cont:index:Index3))]).


gives --->  (word,
             phon:[(a_ gives)],
	     synsem:loc:(cat:(head:(verb,
                                   vform:fin,
                                   pred:plus,
                                   aux:minus),
                                   subcat:[(loc:cont:(psoa;index:(pers:third,
                                                                   num:sg)))|_]),
                        cont:(quants:[],
			      nucleus:(give_rel,
                                      giver:Index1,
                                      gift:Index2,
                                      given:Index3))),
            qstore:[],
            retr:[],
            arg_st:[(loc:(cat:(head:noun,
                               subcat:[]),
                          cont:index:Index1)),
                     (loc:(cat:(head:noun,
                                subcat:[]),
                          cont:index:Index2)),
                     (loc:(cat:(head:(prep,
                                      pform:to),
                                subcat:[]),
                          cont:index:Index3))]).


%%% AUXILIARIES

% LE of will (future auxiliary)

will ---> (word,
           phon:[(a_ will)],
	   synsem:loc:(cat:head:(verb,
                                vform:fin,
                                pred:plus,
                                aux:plus),
                      cont:(quants:[],
		            nucleus:(future_rel,
                                    soa_arg:Cont))),
	   qstore:[],
	   retr:[],
      arg_st:[(Synsem),(loc:(cat:(head:(verb,
                                        vform:base),
                                  subcat:[(Synsem)]),
                                  cont:Cont))]).


% PREPOSITIONS

to ---> (word,
         phon:[(a_ to)],
	 synsem:loc:cat:head:(prep,
                              pform:to)).

%to ---> (word,
%        phon:[(a_ to)],
%        synsem:loc:(cat:head:(prep,
%                               mod:(loc:(cat:(head:verb,
%                                              subcat:ne_list),
%                                         cont:nucleus:(Cont1))),
%                               pred:minus,
%                               pform:lexical),
%                          cont:(quants:[],
%			        nucleus:(direction_rel,
%                                        movement:Cont1,
%                                        goal:Cont2))),
%	qstore:[],
%	retr:[],
%        arg_st:[(loc:(cat:(head:noun,
%                           subcat:[]),
%                      cont:index:Cont2))]).


% ADVERBS

here ---> (word,
           phon:[(a_ here)],
	   synsem:loc:(cat:head:(adv,
                                pred:minus,
                                mod:loc:(cat:(head:verb,
                                              subcat:ne_list),
                                         cont:Cont)),
                      cont:(quants:[],
		           nucleus:(here_rel,
                                    located:Cont))),
	   qstore:[],
	   retr:[],
           arg_st:[]).


% COMPLEMENTIZERS

that ---> (word,
           phon:[(a_ that)],
	   synsem:loc:(cat:(head:(mark,
                                  spec:(loc:(cat:(head:(verb,
                                                        vform:fin),
						  marking:unmarked,
                                                  subcat:[]),
                                              cont:Cont))),
                            marking:that),
                       cont:Cont),
          arg_st:[],
	  qstore:[],
	  retr:[]).




%====================================================================




