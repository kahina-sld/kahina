package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable.State;

public class LearnMoreAnalyseConflict implements IAnalyseConflict {
	public static double ACTIVITYGAIN = 25;
	public static double ACTIVITYCONFVAR = 25;
	public int LearnClauseLimit = 15;
	//TODO: Eine weitere belegung rückgängig machen wenn zwei Klauseln gelernt werden sollen.
	private CDCL cdcl;

	public LearnMoreAnalyseConflict(){
	}
	/**
	 * waehlt die Variable aus mit deren LearnClauseLimitreason als naechstes resolviert werden
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
		//		System.out.println(lv.num);
		Clause reason = lv.reason;
		lv.unassign(cdcl.activity);
		lv.reason = null;
		// erste resolution
		Clause newClause = cdcl.resolve(conflict, reason);

		Clause c1 = reason;
		Clause c2 = conflict;

		boolean c1HasBeenResolved = false;
		boolean c2HasBeenResolved = false;
		// resolviere so lange bis die resolvierende Klausel 1UIP ist
		while (!is1UIP(newClause)) {
			final Variable cv = chooseLiteral(newClause);
			reason = cv.reason;
			cv.reason = null;
			
			cv.activity += ACTIVITYGAIN;

			final int v = cv.state==State.FALSE?cv.num:-cv.num;

			//TODO resolve funktion die nur noetige klauseln auf subsumierungen ueberprüft.
			//			System.out.println(""+ v + reason + c1);
			if (c1.literals.contains(v)){
				c1 = cdcl.resolve2(c1, reason,v);
				c1HasBeenResolved = true;
			}
			
			if (c2.literals.contains(v)){
				c2 = cdcl.resolve2(c2, reason,v);
				c2HasBeenResolved = true;
			}

			//			if (is1UIP(c1)){
			////				System.out.println("Huray");
			////				cdcl.addLater.add(c2);
			////				if (reason.literals.contains(v)){
			////					System.out.println("wtf");
			////				}
			//				cdcl.addLater.add(c1);
			////				System.out.println(c2 + " : " + c1 + " vs " + newClause);
			////				return cdcl.computeBacktrackLevel(c1);
			//			}

			//			if (is1UIP(c2)){
			////				System.out.println("Huray");
			////				cdcl.addLater.add(c1);
			//				cdcl.addLater.add(c2);
			////				System.out.println(c1 + " : " + c2 + " vs " + newClause);
			////				return cdcl.computeBacktrackLevel(c2);
			//			}


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

		if (c1HasBeenResolved){
			resolveIfShortsClause2(c1);
			if (newClause.literals.size()-1 > c1.literals.size() &&  !cdcl.addLater.contains(c1) && c1.literals.size() < LearnClauseLimit) {
				cdcl.addLater.add(c1);

//				if (c1.literals.contains(lv.num)){
////					System.out.println("true");
//					lv.setNext = State.FALSE;
//				}else{
////					System.out.println("false");
//					lv.setNext = State.TRUE;
//				}
				
				lv.activity += ACTIVITYCONFVAR;
				
//				System.err.println(newClause + " vs " + c1);
			}	
		}

		if (c2HasBeenResolved){
			resolveIfShortsClause2(c2);
			if (newClause.literals.size()/2 > c2.literals.size() &&!cdcl.addLater.contains(c2) && c1.literals.size() < LearnClauseLimit) {
				cdcl.addLater.add(c2);


//				if (c2.literals.contains(lv.num)){
////					System.out.println("true");
//					lv.setNext = State.FALSE;
//				}else{
////					System.out.println("false");
//					lv.setNext = State.TRUE;
//				}
				
				lv.activity += ACTIVITYCONFVAR;
//				System.err.println(newClause + " vs " + c2);
			}
		}


		// die neue klausel wird spaeter hinzugefuegt
		//		if (!cdcl.addLater.contains(newClause)) {
		cdcl.addUnit.add(newClause);
		cdcl.addLater.remove(newClause);
		//		}




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


	//	private boolean has2openLiterals(Clause c1) {
	//
	//		
	//		return false;
	//	}

	public void resolveIfShortsClause2(final Clause newClause) {

		int size = newClause.literals.size();
		for (int i = 0; i < size; i++) {
			final int litNewClause = newClause.literals.get(i);
			final Variable v = cdcl.instance.variables[Math.abs(litNewClause)];
			final Clause reason = v.reason;
			boolean vOK = (v.state==State.FALSE)?(litNewClause > 0):(litNewClause < 0);

//			if (!vOK){
//			if (newClause.equals(reason)){
//				System.err.println("Should not happen!!!!");
//			}

			final int reasonSize;
			if (reason != null && (reasonSize = reason.literals.size()) <= size && vOK ) {

				boolean delete = true;
				boolean compl = false;
				for (final int l : reason.literals) {
					if (!newClause.literals.contains(l)) {
						if (l != -litNewClause) {
							delete = false;
							break;
						} else {
							if (!compl) {
								compl = true;
							} else {
								delete = false;
								break;
							}
						}
					}
				}

				if (delete) {
//					System.err.println("1:" + newClause + " > " + reason);
					newClause.literals.remove(i);
//					System.err.println(newClause);
					// if (newClause.toString().equals("{ -29 95 -48 -18 }")){
					// System.out.println("for the breakpoint");
					// }
					size--;
					// newClause subsumiert reason
					if (size < reasonSize){
						cdcl.removeClause(reason);
						System.err.println("2:" + size + " " + newClause + " < " + reason);
						//TODO set min backjump value
					}
				}
			}
		}
	}
}
