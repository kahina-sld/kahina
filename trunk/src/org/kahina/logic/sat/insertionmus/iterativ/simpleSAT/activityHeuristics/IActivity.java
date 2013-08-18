package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;

/**
 * Ein Interface dass es ermoeglicht die Aktivity Heuristik auf unterschiedliche arten zu implementieren
 * @author seitz
 *
 */
public interface IActivity {

	void NewUnitClause(Clause newClause);
	void NewClause(Clause newClause);

	Variable getNextVar();

	void unasignedVariable(Variable variable);

	void increaseActivity(Variable v, float Gain);
	void init(ClauseSet set);


}
