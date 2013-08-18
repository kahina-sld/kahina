package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause;

public interface IAnalyseConflict {
	/**
	 * resolviert eine Konfliktklausel nach dem 1UIP prinzip. anschliesend wird
	 * sie weitmoeglichst vereinfacht. falls waerend diesem vorgang
	 * subsumierungen gefunden werden, werden entsprechend Klauseln geloescht.
	 * 
	 * @return das neue Level (das hoechste level einer belegten Variable die in
	 *         der aus der Konfliktklausel resolvierenden Klausel enthalten ist)
	 */
	public int analyseConflict(final Clause conflict);
	public void init(CDCL cdcl);
}
