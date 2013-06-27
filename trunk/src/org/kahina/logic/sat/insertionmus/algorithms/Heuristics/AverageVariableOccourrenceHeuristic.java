package org.kahina.logic.sat.insertionmus.algorithms.Heuristics;

import java.util.Comparator;
import java.util.List;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class AverageVariableOccourrenceHeuristic implements ISortingHeuristic{

//	private CnfSatInstance instance;

	private ISortingHeuristic fallback;

	public AverageVariableOccourrenceHeuristic(ISortingHeuristic fallback){

		this.fallback = fallback;
	}
	
	@Override
	public Comparator<Integer> getComparator(final CnfSatInstance instance) {

		return new Comparator<Integer>(){

			@Override
			public int compare(Integer arg0, Integer arg1) {
				List<Integer> clause1 = instance.getClauseByID(arg0);
				List<Integer> clause2 = instance.getClauseByID(arg1);
				float weightC1 = 0;
				float weightC2 = 0;
				for (int variableID: clause1){
					weightC1 += instance.getCountOccourrence(variableID);
				}
				weightC1 = (weightC1/clause1.size())*10000;
				for (int variableID: clause2){
					weightC2 += instance.getCountOccourrence(variableID);
				}
				weightC2 = (weightC2/clause2.size())*10000;
				
				int ret = (int) (weightC1 - weightC2);
				if (ret == 0){
					Comparator<Integer> cp = fallback.getComparator(instance);
					return cp.compare(arg0, arg1);
				}
				return ret;
			}};
	}
	
	@Override
	public String toString(){
		return "Average Literal Occourrence" ;
	}

}
