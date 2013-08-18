package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics;

import java.util.Comparator;
import java.util.PriorityQueue;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable.State;


public abstract class AbstractActivity  implements IActivity{
	

	public int countTillActivityReduction = 1000;
	public double activityReduction = 0.1d;
	public double unitActivityGain = 30; // Variable kommt in einer Resolvierten Klausel vor;
	public double clauseActivityGain = 5;

	protected PriorityQueue<Variable> pq;
	protected ClauseSet instance;
	public int count = 0;
	
	
	
	public AbstractActivity() {

	}
	
	@Override
	public void init(ClauseSet set){
		this.instance = set;
		pq = new PriorityQueue<Variable>(set.variables.length,varcomp);
	
		for (int i = set.variables.length -1; i > 0; i--){
			pq.add(set.variables[i]);
		}
		
	}
	
	final Comparator<Variable> varcomp = new Comparator<Variable>(){
		@Override
		public int compare(Variable o1, Variable o2) {
			return (o1.activity > o2.activity ? -1 :  (o1.activity < o2.activity ? 1 : 0));
		}};

	



//	public AbstractActivity(int countTillActivityReduction, int count,
//			float activityReduction, float activityGain,
//			Comparator<Variable> varcomp) {
//		this.countTillActivityReduction = countTillActivityReduction;
//		this.count = count;
//		this.activityReduction = activityReduction;
//		this.activityGain = activityGain;
//		this.varcomp = varcomp;
//	}
	
	@Override
	public void NewUnitClause(Clause newClause) {

		for (final int l: newClause.literals){
			final Variable v = this.instance.variables[Math.abs(l)];
			
			if (v.state == State.OPEN){
				this.pq.remove(v);
				v.activity += this.unitActivityGain;
				this.pq.add(v);
			}else{
				v.activity += this.unitActivityGain;
			}
		}	
	}
	

	@Override
	public void NewClause(Clause newClause) {

		for (final int l: newClause.literals){
			final Variable v = this.instance.variables[Math.abs(l)];
			
			if (v.state == State.OPEN){
				this.pq.remove(v);
				v.activity += this.clauseActivityGain;
				this.pq.add(v);
			}else{
				v.activity += this.clauseActivityGain;
			}
		}	
	}
	

	@Override
	public void increaseActivity(Variable v, float Gain){
		if (v.state == State.OPEN){
			this.pq.remove(v);
			v.activity += Gain;
			this.pq.add(v);
		}else{
			v.activity += Gain;
		}
	}

	@Override
	public Variable getNextVar() {
		count++;
		if (count >= this.countTillActivityReduction){
			count = 0;
			for (final Variable var: this.instance.variables){
				var.activity *= activityReduction;
			}
		}
		
		
		Variable v;
		do{
			v= this.pq.poll();
		}while (v.state != State.OPEN);
		
		return v;
	}

	@Override
	public void unasignedVariable(Variable variable) {
		this.pq.add(variable);
	}
}