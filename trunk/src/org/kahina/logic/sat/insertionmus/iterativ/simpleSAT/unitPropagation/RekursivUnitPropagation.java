package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation;

import java.util.ArrayList;
import java.util.Stack;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;


public class RekursivUnitPropagation implements IUnitPropagation {

	private final ClauseSet instance;

	public RekursivUnitPropagation(final ClauseSet set){
		// if (newClause.toString().equals("{ -29 95 6 }")){
		// System.out.println("For the breakpoint, analyseConflict");
		// }
		this.instance = set;
	}

	
	@Override
	public Clause unitPropagation(final Stack<Variable> stack,final int lvl) {
		final ArrayList<Clause> newUnits = new ArrayList<Clause>();

		for (final Clause c : instance.initUnits){
			int lit = c.getUnassigned( instance.variables);		  

			if (lit != 0){
				final Variable v = instance.variables[Math.abs(lit)];
				v.reason = c;
	
				final Clause empty = v.assign(lit > 0, instance.variables, newUnits, stack, lvl);
				if (empty != null) {
					return empty;
				}
			}
		}

		instance.initUnits = newUnits;

		if ( instance.initUnits.size() > 0) return this.unitPropagation(stack, lvl);
		return null;
	}
	
}
