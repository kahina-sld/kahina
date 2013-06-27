package org.kahina.logic.sat.insertionmus.algorithms.Heuristics;

import java.util.Comparator;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public interface ISortingHeuristic {
	Comparator<Integer> getComparator(final CnfSatInstance instance);
}
