package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.StaticHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.DefaultAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.LearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation.UnitPropagation;


public class DefaultFactory implements ICDCLFactory{
	@Override
	public CDCL getCDCL(ClauseSet set) {
		return new CDCL(set);
//		return new CDCL(set, new StaticHeapActivity(set), new UnitPropagation(set), new DefaultAnalyseConflict());
		
	}
}
