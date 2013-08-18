package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;

public class DefaultAnalyseConflict implements IAnalyseConflict {

	private CDCL cdcl;

	public DefaultAnalyseConflict(){
	}
	/**
	 * waehlt die Variable aus mit deren reason als naechstes resolviert werden
	 * soll
	 */
	private Variable chooseLiteral(final Clause newClause) {

		while (true) {
			final Variable v = cdcl.stack.pop();

			final int till = newClause.literals.size();
			for (int i = 0; i < till; i++) {
				final int l = newClause.literals.get(i);
				if (v.num == Math.abs(l)) {
					newClause.literals.set(i, newClause.literals.get(0));
					newClause.literals.set(0, l);

					v.unassign(cdcl.activity);
					return v;
				}
			}
			v.unassign(cdcl.activity);
			v.reason = null;
		}
	}
	/**
	 * Ist die uebergebende variable bereits 1UIP, dh. genau eine Variable ist
	 * maxlevel
	 */
	private boolean is1UIP(final Clause c) {
		int numOfMaxLvl = 0;
		for (final int i : c.literals) {
			if (cdcl.instance.variables[Math.abs(i)].level == cdcl.lvl)
				numOfMaxLvl++;
		}
		return (numOfMaxLvl < 2);
	}
	
	@Override
	public int analyseConflict(Clause conflict) {
		
		if (cdcl.lvl == 0) { // UNSAT
			return -1;
		}

		// reason fuer den conflict
		final Variable lv = cdcl.stack.pop();
		Clause reason = lv.reason;
		lv.unassign(cdcl.activity);
		lv.reason = null;
		// erste resolution
		Clause newClause = cdcl.resolve(conflict, reason);
		// resolviere so lange bis die resolvierende Klausel 1UIP ist
		while (!is1UIP(newClause)) {
			final Variable cv = chooseLiteral(newClause);
			reason = cv.reason;
			cv.reason = null;
			newClause = cdcl.resolveC1Begin(newClause, reason);
		}

		// if (newClause.toString().equals("{ -29 95 6 }")){
		// System.out.println("For the breakpoint, analyseConflict");
		// System.out.println(stack);
		// }

		// versuche die aktuelle klausel zu verkleinern.
		// (in den uns zur verfuegung gestellten instanzen hat dies ein einziges
		// mahl geklappt)
		cdcl.resolveIfShortsClause(newClause);

		// die neue klausel wird spaeter hinzugefuegt
		if (!cdcl.addUnit.contains(newClause)) {
			cdcl.addUnit.add(newClause);
			cdcl.addLater.remove(newClause);
		}


		// Restart if unit clause
		if (newClause.literals.size() == 1) {
			return 0;
		}

		return cdcl.computeBacktrackLevel(newClause);
	}
	@Override
	public void init(CDCL cdcl) {
		this.cdcl = cdcl;
	}

}
