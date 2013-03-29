% Definite Clauses
:- multifile if/2.


% variants of append/3 with different delays
% ------------------------------------------

% no_del_append/3: no delays

no_del_append(L,[],L) if true.
no_del_append([],(L,ne_list),L) if true.
no_del_append([H|T1],(L,ne_list),[H|T2]) if
  no_del_append(T1,L,T2).

% first_arg_append/3 with delay on the first argument

first_arg_append(X,Y,Z) if
   when(  ( X=(e_list;ne_list)
                ),
            undelayed_append(X,Y,Z)).

undelayed_append(L,[],L) if true.
undelayed_append([],(L,ne_list),L) if true.
undelayed_append([H|T1],(L,ne_list),[H|T2]) if
  first_arg_append(T1,L,T2).



% phon list predicates
% --------------------

% collect_phons/2
% collect list of phonologies of any number of complement daughters

collect_phons([],[]) if true.
collect_phons([(phon:Phon)|Tlsigns],ListofPhons) if
                                        collect_phons(Tlsigns,Tlphons),
					no_del_append(Phon,Tlphons,ListofPhons).

% phon_append/3
% functionally append the daughter phonologies to obtain the
% phonology of the mother node (delayed for mother-leftmost-daughter
% indexing in ALE)

fun phon_append(+,+,-).

phon_append(X,Y,Z) if
   when(  ( X=(e_list;ne_list),
	    Y=(e_list;ne_list)
               ),
	  undelayed_phon_append(X,Y,Z) ).

undelayed_phon_append([],[],[]) if true.
undelayed_phon_append([],[H|T1],[H|T2]) if phon_append([],T1,T2).
undelayed_phon_append([H|T1],L,[H|T2]) if phon_append(T1,L,T2).


% Subcategorization Principle
% ---------------------------

sign_ss_append(X,Y,Z) if
   when(  ( % X=(e_list;ne_list),
            Y=(e_list;ne_list)
          , Z=(e_list;ne_list)
                ),
            undel_sign_ss_append(X,Y,Z)).

undel_sign_ss_append(L,[],L) if true.
undel_sign_ss_append([],[(synsem:First)|Tl1],[First|Tl2]) if
                                    sign_ss_append([],Tl1,Tl2).
undel_sign_ss_append([Hd|T1],(L,ne_list),[Hd|T2]) if
                                    sign_ss_append(T1,L,T2).



% Qstore Predicates
% -----------------

% collect_dtrs_qstores/2
% collect the qstores of any number of daughters of known construction C1 in
% new list L2
% no delays on append/3 since the daughters and their qstores are known

collect_dtrs_qstores((head_comp_struc,
		      c_dtrs:Cdtrs,
		      hdtr:qstore:Q2),
		      Q3) if (collect_qstores(Cdtrs,Q1),
                              no_del_append(Q1,Q2,Q3)).
collect_dtrs_qstores((head_mark_struc,
		      marker_dtr:qstore:Q1,
		      hdtr:qstore:Q2),
		      Q3) if no_del_append(Q1,Q2,Q3).
collect_dtrs_qstores((head_adj_struc,
		      adj_dtr:qstore:Q1,
		      hdtr:qstore:Q2),
		      Q3) if no_del_append(Q1,Q2,Q3).


% collect_qstores/2
% union the qstores of a known list of signs S1 in the new list L2
% no delays on append/3 since the daughters and their qstores are known

collect_qstores([],[]) if true.
collect_qstores([(qstore:Qstore)|Tlsigns],ListofQstores) if
                             (collect_qstores(Tlsigns,TlQstores),
	                      no_del_append(Qstore,TlQstores,ListofQstores)).



% Quantifier Retrieval

% qstore_retrieval(Retr,Qstore,Collection)
% split the known list of quantifiers in Collection into two in such a
% way that Retr contains any number of them in any order and Qstore
% contains all the others in the original order from Collection

% case 1: nothing to retrieve
% case 2: nothing is retrieved but something could be
% case 3: something is retrieved

qstore_retrieval([],[],[]) if true.
qstore_retrieval([],(List,ne_list),List) if true.
qstore_retrieval((R,ne_list),Q,(List,ne_list)) if
                                select_any_number_order(List,R,Q).


% select_any_number_order(List,Retr,Qstore)
% given a known list of quantifiers List, let Retr be a subset of them
% and Qstore whatever is left

select_any_number_order(Input,[],Input) if true.
select_any_number_order(Input,[Ele|SInput],Restlist) if
                              select_one(Ele,Input,Rest),
		              select_any_number_order(Rest,SInput,Restlist).


select_one(Ele,[Ele|Rest],Rest) if true.
select_one(Ele,[Hd|Tail],[Hd|Rest]) if select_one(Ele,Tail,Rest).



% Collect the contextual backgrounds of a list of complements

% collect_cdtrs_backgrounds/2
% union the backgrounds of a known list of signs S1 in the new list B2
% no delays on append/3 since the daughters and their backgrounds are known

collect_cdtrs_backgrounds([],[]) if true.
collect_cdtrs_backgrounds([(synsem:loc:context:background:B)|Tlsigns],LBs) if
            (collect_cdtrs_backgrounds(Tlsigns,TlBs),
	     no_del_append(B,TlBs,LBs)).