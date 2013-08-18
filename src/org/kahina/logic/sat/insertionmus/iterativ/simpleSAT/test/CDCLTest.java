package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.test;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.NonRandomHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.RandomHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.DisabledLearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.LearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.ICDCLFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Solution;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation.UnitPropagation;





import junit.framework.TestCase;

public class CDCLTest extends TestCase {



	ICDCLFactory defaultCDCL = new ICDCLFactory(){
		@Override
		public CDCL getCDCL(ClauseSet set) {
			CDCL cdcl =  new CDCL(set);

//			cdcl.setValues(53857, 8, 0.4561274033516342, 157.02429255361034,
//					147.13784826416332, 141.6777838620123);

			//countTill: IntegerGene(0,100000)=53857
			//clauseLimit: IntegerGene(0,300)=8
			//actReduction: DoubleGene(0.0,0.99999999)=0.4561274033516342
			//actGain: DoubleGene(0.0,200.0)=157.02429255361034
			//initActGain: DoubleGene(0.0,200.0)=147.13784826416332
			//clActGain: DoubleGene(0.0,200.0)=141.6777838620123
			return cdcl;
		}
	};
	ICDCLFactory learnMoreCDCL = new ICDCLFactory(){
		@Override
		public CDCL getCDCL(ClauseSet set) {
			return new CDCL(set, new NonRandomHeapActivity(), new UnitPropagation(set), new LearnMoreAnalyseConflict());
		}
	};

	ICDCLFactory disabledLearnMoreCDCL = new ICDCLFactory(){
		@Override
		public CDCL getCDCL(ClauseSet set) {
			return new CDCL(set, new NonRandomHeapActivity(), new UnitPropagation(set), new DisabledLearnMoreAnalyseConflict());
		}
	};

	public void testTrueMoreClauses(){
		testTrue(learnMoreCDCL);
	}
	public void testFalseMoreClauses(){
		testFalse(learnMoreCDCL);
	}

	public void testTrueDefault(){
		testTrue(defaultCDCL);
	}
	public void testFalseDefault(){
		testFalse(defaultCDCL);
	}

	public void testTrueDisabled(){
		testTrue(disabledLearnMoreCDCL);
	}
	public void testFalseDisabled(){
		testFalse(disabledLearnMoreCDCL);
	}

	//	public void testTrueMoreClauses2(){
	//		testTrue(learnMoreCDCL);
	//	}
	//	public void testTrueDefault2(){
	//		testTrue(defaultCDCL);
	//	}
	//	public void testTrueMoreClauses3(){
	//		testTrue(learnMoreCDCL);
	//	}

	final String path = "../cnf/";
	public void testTrue(ICDCLFactory factory){
		testInstances(path, new FileFilter(){
			@Override
			public boolean accept(File arg0) {
				return arg0.getName().contains("yes");
			}}, Solution.SAT, factory);
	}

	public void testFalse(ICDCLFactory factory){
		testInstances(path, new FileFilter(){
			@Override
			public boolean accept(File arg0) {
				return arg0.getName().contains("no");
			}}, Solution.UNSAT,  factory);
	}


	public void testInstances(String args, FileFilter filter, Solution expectedValue, ICDCLFactory factory) {    
		File f = new File(args);
		if (f.isDirectory()){

			//Finde Alle Dateien im Verzeichnis.
			File[] fileArray = f.listFiles(filter);

			//Starte fuer alle gefundenenn Dateien das Entsprechende Programm
			for (int i = 0; i < fileArray.length; i++){
				//					instance.startInstance.start(fileArray[i], args);
				System.out.println(fileArray[i].getAbsolutePath());
				ClauseSet set = new ClauseSet();
				CDCL cdcl = factory.getCDCL(set);//new CDCL(set);

				//				cdcl.setValues(43934, 187, 9.70572706070727, 4.897113562923039, 72.81629127856833, 40.086757695584275);

				try {
					set.pars(fileArray[i].getAbsolutePath() );
				} catch (NumberFormatException e) {
					fail(e.toString());
					break;
				} catch (IOException e) {
					fail(e.toString());
					break;
				}
				assertEquals(expectedValue,cdcl.solve(220000));
			}

		}else{
			fail(f.getName() + " is not a directory.");
		}
	}
}
