% Multifile declarations.
:- multifile if/2.
:- multifile '*>'/2.

% specify signature file
signature(signature).

:- [grale_specs].
:- [lexicon].
:- [definite_clauses].
:- [contextual_consistency].
:- [agreement_theory].


%====================================================================
% Phrase Structure Rules, encoding the ID-Principle and the
%                                      Constituent Order Principle

% Schema 1: Head Subject Rule

head_subject_schema rule
(phrase,
 phon:phon_append(CPhon,HdPhon),
 synsem:(loc:cat:(head:((inv:minus);(functional;non_verb_subst)),
		  subcat:[])),
 daughters:(head_comp_struc,
            c_dtrs:[(Cdtr)],
            hdtr:(Hdtr, phrase)))
   ===>
cat> (Cdtr, phon:CPhon),
cat> (Hdtr, phon:HdPhon).


% Schema 2: Head Complement Rule

head_complement_schema rule
(phrase,
 phon:phon_append(HdPhon,CompsPhon),
 synsem:(loc:cat:(head:((inv:minus);(functional;non_verb_subst)),
		  subcat:[_])),
 daughters:(head_comp_struc,
            hdtr:(Hdtr, word),
            c_dtrs:Ndtrs))
   ===>
cat> (Hdtr, word, phon:HdPhon),
cats> (([];[_];[_,_]), Ndtrs),
goal> collect_phons(Ndtrs,CompsPhon).


% Schema 2': Prep-Head Complement Rule (fixing a mistake by P&S:94, who
%                                       exclude this construction)

head_complement_schema2 rule
(phrase,
 phon:phon_append(HdPhon,CompsPhon),
 synsem:(loc:cat:(head:prep,
		  subcat:[])),
 daughters:(head_comp_struc,
            hdtr:(Hdtr, word),
            c_dtrs:Ndtrs))
   ===>
cat> (synsem:loc:cat:head:prep, Hdtr, word, phon:HdPhon),
cats> ([phon:CompsPhon], Ndtrs).


% Schema 3: Head-Subject-Complement Rule

head_subj_comp_schema rule
(phrase,
 phon:phon_append(HdPhon,CompsPhon),
 synsem:loc:cat:(head:inv:plus,
		 subcat:[]),
 daughters:(head_comp_struc,
            hdtr:(Hdtr, word),
            c_dtrs:Ndtrs))
   ===>
cat> (synsem:loc:cat:head:inv:plus, Hdtr, word, phon:HdPhon),
cats> (([_];[_,_]), Ndtrs),
goal> collect_phons(Ndtrs,CompsPhon).	    


% Schema 4: Head Marker Rule

head_marker_schema rule
(phrase,
 phon:phon_append(MPhon,HdPhon),
 synsem:loc:cat:subcat:[],
 daughters:(head_mark_struc,
            marker_dtr:Mdtr,          % P&S94: synsem:loc:cat:head:mark
            hdtr:(Hdtr, phrase)))
   ===>
cat> (synsem:loc:cat:head:mark, Mdtr, word, phon:MPhon),
cat> (Hdtr, phon:HdPhon).


% Schema 5: Head Adjunct Rule

head_adjunct_schema rule
(phrase,
 phon:phon_append(APhon,HdPhon),
 daughters:(head_adj_struc,
            adj_dtr:Adtr,       % P&S94: synsem:loc:cat:head:mod:Synsem
            hdtr:Hdtr))         % P&S94: synsem:Synsem
   ===>
cat> (synsem:loc:cat:(head:(adj,	% adj added
			    pred:minus, % added (adj-noun combination)
			    mod:Synsem)),
      Adtr, phon:APhon),
cat> (synsem:Synsem, Hdtr, phon:HdPhon).



% Schema 5: Head Adjunct Rule
% modified, non-recursive

%head_adjunct_rule rule
%(phrase,
% phon:phon_append(APhon,HdPhon),
% synsem:loc:cat:(head:noun,
%		 subcat:[_]),
% daughters:(head_adj_struc,
%            adj_dtr:Adtr,       % P&S94: synsem:loc:cat:head:mod:Synsem
%            hdtr:Hdtr))         % P&S94: synsem:Synsem
%   ===>
%cat> (synsem:loc:cat:(head:(adj,	% adj added
%			    mod:Synsem),
%		      subcat:[]),
%      Adtr, phon:APhon),
%cat> (synsem:Synsem, Hdtr, phon:HdPhon, synsem:loc:cat:subcat:[_]).


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

% remark: second clause specialized from [(...)|_] in antecedent, since
%         that constraint does not fire


% Semantics Principle

% Clause a

%(phrase,
% daughters:headed_struc) *>
(phrase,
 daughters:((head_adj_struc;head_mark_struc),
	    hdtr:(qstore:(e_list;ne_list),
		  retr:(e_list;ne_list)))) *>
(qstore:Qstore,
 retr:Retr,
 daughters:Const_struc)
goal (collect_dtrs_qstores(Const_struc,CollectedQstore),
      qstore_retrieval(Retr,Qstore,CollectedQstore)).

(phrase,
 daughters:(head_comp_struc,
	    hdtr:(qstore:(e_list;ne_list),
		  retr:(e_list;ne_list)),
	    c_dtrs:([];
	            hd:(qstore:(e_list;(hd:quant)),
		        retr:(e_list;(hd:quant)))))) *>
(qstore:Qstore,
 retr:Retr,
 daughters:Const_struc)
goal (collect_dtrs_qstores(Const_struc,CollectedQstore),
      qstore_retrieval(Retr,Qstore,CollectedQstore)).



% Clause b, Conj 1, Disj 1

(phrase,
 daughters:(head_adj_struc,
	    adj_dtr:synsem:loc:cont:psoa)) *>
(synsem:loc:cont:(quants:QuantsM,
		  nucleus:Nucleus),
 retr:(Retr),
 daughters:adj_dtr:synsem:loc:cont:(quants:QuantsD,
		        	    nucleus:Nucleus))
goal first_arg_append(Retr,QuantsD,QuantsM).


% Clause b, Conj 1, Disj 2

(phrase,
 daughters:(non_head_adj_struc,
	    hdtr:synsem:loc:cont:psoa)) *>
(synsem:loc:cont:(quants:QuantsM,
		  nucleus:Nucleus),
 retr:(Retr),
 daughters:hdtr:synsem:loc:cont:(quants:QuantsD,
				 nucleus:Nucleus))
goal first_arg_append(Retr,QuantsD,QuantsM).


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

(head:verb,subcat:e_list) *> head:vform:(fin;base).


% Case Principles

% Structural Case Principle
% Assign nominative to syntactically realized subjects of verbs with
% vform base

NP^(phrase,
 synsem:loc:cat:subcat:e_list,
 daughters:(hdtr:synsem:loc:cat:(head:vform:base,
		 		subcat:hd:(NP,loc:cat:head:noun)),
	    c_dtrs:hd:synsem:NP)) *>
daughters:hdtr:synsem:loc:cat:subcat:hd:loc:cat:head:case:nom.


% Finite Auxiliary Nominative Case Assignment

(word,
 synsem:loc:cat:head:(aux:plus,
		      vform:fin),
 arg_st:hd:loc:cat:head:noun) *>
(arg_st:hd:loc:cat:head:case:nom).



% Lexical Marking Value Principle

(word,
 synsem:loc:cat:head:(subst;det)) *>
 synsem:loc:cat:marking:unmarked.


% Verbal Contextual Background Principle
% Verbs do not introduce background restrictions

(word,
 synsem:loc:cat:head:verb) *>
 synsem:loc:context:background:[].

%====================================================================

