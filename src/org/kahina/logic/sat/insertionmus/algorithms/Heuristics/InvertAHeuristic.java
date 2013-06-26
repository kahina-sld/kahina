package org.kahina.logic.sat.insertionmus.algorithms.Heuristics;

import java.util.Comparator;

public class InvertAHeuristic implements ISortingHeuristic {
	
	protected ISortingHeuristic heuristic;
	public InvertAHeuristic(ISortingHeuristic heuristic){
		this.heuristic = heuristic;
	}
	
	@Override
	public Comparator<Integer> getComparator() {
		final Comparator<Integer> comp = heuristic.getComparator();
		return new Comparator<Integer>(){

			@Override
			public int compare(Integer o1, Integer o2) {
				return -comp.compare(o1, o2);
			}
			
		};
	}

}
