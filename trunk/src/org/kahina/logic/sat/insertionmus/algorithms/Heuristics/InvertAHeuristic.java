package org.kahina.logic.sat.insertionmus.algorithms.Heuristics;

import java.util.Comparator;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class InvertAHeuristic implements ISortingHeuristic {
	
	protected ISortingHeuristic heuristic;
	public InvertAHeuristic(ISortingHeuristic heuristic){
		this.heuristic = heuristic;
	}
	
	@Override
	public Comparator<Integer> getComparator(final CnfSatInstance instance) {
		final Comparator<Integer> comp = heuristic.getComparator(instance);
		return new Comparator<Integer>(){

			@Override
			public int compare(Integer o1, Integer o2) {
				return comp.compare(o2, o1);
			}
			
		};
	}
	

	@Override
	public String toString(){
		return "Inverted " + heuristic.toString() ;
	}


}
