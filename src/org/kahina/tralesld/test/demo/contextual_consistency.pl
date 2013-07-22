:- multifile '*>'/2.

% Principle of Contextual Consistency
% Split into cases with complex antecedents to delay execution until
% the context backgrounds of all daughters are known

% Head Adjunct Structures

(phrase,
 daughters:(head_adj_struc,
	    hdtr:(qstore:(e_list;ne_list),
		  retr:(e_list;ne_list)))) *>
(synsem:loc:context:background:Mbgr,
 daughters:(hdtr:synsem:loc:context:background:Hbgr,
	    adj_dtr:synsem:loc:context:background:Abgr))
goal when( ( Hbgr = (e_list;ne_list),
	     Abgr = (e_list;ne_list)
	    ),
            no_del_append(Hbgr,Abgr,Mbgr)).


% Head Complement Structures

(phrase,
 daughters:(head_comp_struc,
	    hdtr:synsem:loc:context:background:(e_list;ne_list),
            c_dtrs:([];
		    hd:synsem:loc:context:background:(e_list;ne_list)))) *>
(synsem:loc:context:background:Mbgr,
 daughters:(hdtr:synsem:loc:context:background:Hbgr,
	    c_dtrs:Cdtrs))
goal (collect_cdtrs_backgrounds(Cdtrs,Cbgr),
      no_del_append(Hbgr,Cbgr,Mbgr)).


% Head Marker Structures

(phrase,
 daughters:head_mark_struc) *>
%	    hdtr:synsem:loc:context:background:(e_list;ne_list),
%	    marker_dtr:synsem:loc:context:background:(e_list;ne_list))) *>
(synsem:loc:context:background:Mbgr,
 daughters:(hdtr:synsem:loc:context:background:Hbgr,
	    marker_dtr:synsem:loc:context:background:Mabgr))
goal when( ( Hbgr = (e_list;ne_list),
	     Mabgr = (e_list;ne_list)
	    ),
            no_del_append(Hbgr,Mabgr,Mbgr)).