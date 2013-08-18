package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation;

import java.util.ArrayList;
import java.util.Stack;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable.State;



public class UnitPropagation implements IUnitPropagation {

	private final ClauseSet instance;

	public UnitPropagation(final ClauseSet set){
		this.instance = set;
	}

	@Override
	public Clause unitPropagation(final Stack<Variable> stack, final int lvl) {


		while (instance.initUnits.size() > 0){
			ArrayList<Clause> newUnits = new ArrayList<Clause>();

			for (final Clause c : instance.initUnits){

				final int lit = c.getUnassigned(instance.variables);		  
				if (lit == 0){
					//Clausel ist Erfuellt
				}else{
					final Variable v = instance.variables[Math.abs(lit)];
					v.reason = c;

					final Clause empty = v.assign(lit > 0, instance.variables, newUnits, stack, lvl);
					

//					System.out.println(""+ lit + c);
//					if (v.state == State.FALSE && lit > 0 || v.state == State.TRUE && lit<0){
//						System.err.println(""+ lit + c);
//					}
					
					if (empty != null) {
						return empty;//es ist eine leere klausel aufgetreten
					}
				}
			}
			instance.initUnits = newUnits;
		}
		return null;
	}

}
