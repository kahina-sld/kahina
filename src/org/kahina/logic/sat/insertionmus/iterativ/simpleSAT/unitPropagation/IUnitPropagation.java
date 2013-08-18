package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation;

import java.util.Stack;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;


public interface IUnitPropagation {

	/**
	 * Unit Propagation
	 * @param stack 
	 * @param lvl 
	 * 
	 * @return gab es eine Leere Klausel waehrend der UP
	 *            
	 */
	Clause unitPropagation(Stack<Variable> stack, int lvl);

}
