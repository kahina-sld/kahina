package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.optimization;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.NonRandomHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.DisabledLearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.LearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.ICDCLFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.LearnMoreFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation.UnitPropagation;



public class DefaultVSCustom {

	static CDCLFitnessFunctionMultiThreaded ff = new CDCLFitnessFunctionMultiThreaded();
	
	static public void main(String[] arg0) throws NumberFormatException, IOException{
//		ChooseInstance ci = new ChooseInstance("../cnf/SWGCP-allSat-scrambled-first500");
//		ChooseInstance ci = new ChooseInstance("../cnf/test");
		ChooseInstance ci = new ChooseInstance("../cnf/SWGCP-allSat-scrambled-last1000");
		
		System.out.println("Default:");
		
		System.out.println(ff.test());

//		InputStreamReader ir = new InputStreamReader(System.in);
//		BufferedReader br = new BufferedReader(ir);
//		
//		System.out.println("count till:");
//		final int countTill = Integer.valueOf(br.readLine());
//		
//
//		System.out.println("clause limit:");
//		final int clauseLimit = Integer.valueOf(br.readLine());
//
//		System.out.println("actReduction:");
//		final double actReduction = Double.valueOf(br.readLine());
//
//		System.out.println("unitActGain:");
//		final double actGain = Double.valueOf(br.readLine());
//
//		System.out.println("initial ActGain:");
//		final double initActGain = Double.valueOf(br.readLine());
//
//		System.out.println("clActGain:");
//		final double clActGain = Double.valueOf(br.readLine());
//		
//
//		System.out.println("Restard 1/2/3:");
//		final int restardValue1 = Integer.valueOf(br.readLine());
//		final int restardValue2 = Integer.valueOf(br.readLine());
//		final int restardValue3 = Integer.valueOf(br.readLine());
//		
//		System.out.println("restard multiplyer:");
//		final double restardMult = Double.valueOf(br.readLine());
//		
//		ff.factory = new ICDCLFactory(){
//			LearnMoreFactory f = new LearnMoreFactory();
//			@Override
//			public CDCL getCDCL(ClauseSet set) {
//				CDCL cdcl = f.getCDCL(set);
//				cdcl.setValues(countTill, clauseLimit, actReduction,
//						actGain, initActGain, clActGain
//						, restardValue1, restardValue2, restardValue3, restardMult);
//				return cdcl;
//			}};
//		
		ff.factory = new ICDCLFactory() {
			
			@Override
			public CDCL getCDCL(ClauseSet set) {
				return new CDCL(set, new NonRandomHeapActivity(), new UnitPropagation(set), new DisabledLearnMoreAnalyseConflict());
			}
		};
		System.out.println("Disabled: ");
		System.out.println(ff.test());
		
		ff.factory = new ICDCLFactory() {
			
			@Override
			public CDCL getCDCL(ClauseSet set) {
				CDCL cdcl = new CDCL(set, new NonRandomHeapActivity(), new UnitPropagation(set), new DisabledLearnMoreAnalyseConflict());
				cdcl.setValues(4550, 0, 0.5495454115605939, 59.36472382346888,
						51.778740625695754, 46.12130202672618, 102, 8588, 9074,
						5.734719063582333);
				return cdcl;
			}
		};
		System.out.println("Optimised disabled: ");
		LearnMoreAnalyseConflict.ACTIVITYGAIN = 52.121983968223404;
		System.out.println(ff.test());
		
//		ff.factory = new LearnMoreFactory();
//		System.out.println(ff.test());
		LearnMoreAnalyseConflict.ACTIVITYCONFVAR = 33.819334304948995;
		LearnMoreAnalyseConflict.ACTIVITYGAIN = 95.52092354972373;
		ff.factory = new ICDCLFactory() {
			@Override
			public CDCL getCDCL(ClauseSet set) {
				CDCL cdcl = new CDCL(set, new NonRandomHeapActivity(), new UnitPropagation(set), new LearnMoreAnalyseConflict());
				cdcl.setValues(1005, 4, 0.05568172220303892, 26.684718504681538, 50.44917172811717, 
						54.632922233621336, 307, 9384, 8010, 30.730965054013794);
				return cdcl;
			}
		};
		System.out.println("Optimised learn more: ");
		System.out.println(ff.test());

		LearnMoreAnalyseConflict.ACTIVITYCONFVAR = 25;
		LearnMoreAnalyseConflict.ACTIVITYGAIN = 25;
		ff.factory = new LearnMoreFactory();
		System.out.println("Default:");
		System.out.println(ff.test());
	}
}
