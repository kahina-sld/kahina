package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;

public interface ICDCLFactory {

	CDCL getCDCL(ClauseSet set);
}
