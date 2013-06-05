package org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.Heuristics;

import java.util.Comparator;

public class AscendingIndexHeuristic implements ISortingHeuristic {

	@Override
	public Comparator<Integer> getComparator() {
		return new Comparator<Integer>(){
			@Override
			public int compare(Integer i1, Integer i2) {
				return i1-i2;
			}};
	}

}
