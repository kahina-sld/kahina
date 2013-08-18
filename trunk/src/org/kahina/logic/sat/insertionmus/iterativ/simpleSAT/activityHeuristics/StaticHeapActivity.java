package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics;


import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Random;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable.State;


public class StaticHeapActivity extends AbstractActivity{



	@Override
	public void NewUnitClause(Clause newClause) {}
	
	@Override
	public void increaseActivity(Variable v, float Gain){}


}
