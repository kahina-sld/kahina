package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics;


import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Random;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable.State;


public class RandomHeapActivity extends AbstractActivity{

	final Random r = new Random();






	@Override
	public Variable getNextVar() {
		if (r.nextInt()%this.countTillActivityReduction == 0){
			for (final Variable var: this.instance.variables){
				var.activity *= 0.1;
			}
		}
		
		
		Variable v;
		do{
			v= this.pq.poll();
		}while (v.state != State.OPEN);
		
		return v;
	}

//	@Override
//	public void unasignedVariable(Variable variable) {
//		this.pq.add(variable);
//	}
//	
//
//	@Override
//	public void increaseActivity(Variable v, float Gain){
//		if (v.state == State.OPEN){
//			this.pq.remove(v);
//			v.activity += Gain;
//			this.pq.add(v);
//		}else{
//			v.activity += Gain;
//		}
//	}
}
