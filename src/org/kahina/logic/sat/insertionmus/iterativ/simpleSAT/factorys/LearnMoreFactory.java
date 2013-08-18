package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.NonRandomHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.StaticHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.LearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation.UnitPropagation;


public class LearnMoreFactory implements ICDCLFactory{
	@Override
	public CDCL getCDCL(ClauseSet set) {
		return new CDCL(set, new NonRandomHeapActivity(), new UnitPropagation(set), new LearnMoreAnalyseConflict());
	}
}
