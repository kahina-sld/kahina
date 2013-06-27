package org.kahina.logic.sat.insertionmus.algorithms.Heuristics;

import java.util.Comparator;
import java.util.List;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class LargeClausesFirstHeuristic implements ISortingHeuristic {


	private ISortingHeuristic fallback;


	public LargeClausesFirstHeuristic(ISortingHeuristic fallback){
		this.fallback = fallback;
	}
	
	@Override
	public Comparator<Integer> getComparator(final CnfSatInstance instance) {
		return new Comparator<Integer>(){
			@Override
			public int compare(Integer o1, Integer o2) {
				
				int ret = instance.getClauseByID(o2).size() - instance.getClauseByID(o1).size();
				if (ret == 0){
					ret = fallback.getComparator(instance).compare(o1, o2);
				}
				return ret;
			}};
	}
	

	@Override
	public String toString(){
		return "Large clauses first" ;
	}


}
