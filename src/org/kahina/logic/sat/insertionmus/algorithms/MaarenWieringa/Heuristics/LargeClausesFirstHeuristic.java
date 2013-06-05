package org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.Heuristics;

import java.util.Comparator;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class LargeClausesFirstHeuristic implements ISortingHeuristic {

	private CnfSatInstance instance;

	public LargeClausesFirstHeuristic(CnfSatInstance instance){
		this.instance = instance;
	}
	
	@Override
	public Comparator<Integer> getComparator() {
		return new Comparator<Integer>(){
			@Override
			public int compare(Integer o1, Integer o2) {
				return instance.getClauseByID(o1).size() - instance.getClauseByID(o2).size();
			}};
	}

}
