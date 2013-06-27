package org.kahina.logic.sat.insertionmus.algorithms.Heuristics;

import java.util.Comparator;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class AscendingIndexHeuristic implements ISortingHeuristic {

	@Override
	public Comparator<Integer> getComparator(final CnfSatInstance instance) {
		return new Comparator<Integer>(){
			@Override
			public int compare(Integer i1, Integer i2) {
				return i1-i2;
			}};
	}
	
	@Override
	public String toString(){
		return "Ascending index";
	}

}
