package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.util.Arrays;
import java.util.concurrent.TimeoutException;

import junit.framework.TestCase;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.AdvancedAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.FasterAdvancedAlgorithm;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.ResultNotRetrievableException;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;

public class TestAnAlgorithm extends TestCase{
	//	String[] paths = {""};

	public void testBinary() throws TimeoutException, ResultNotRetrievableException{

		testAnAlgorithm(new BinaryAlgorithm());
	}
	public void testAdvanced() throws TimeoutException, ResultNotRetrievableException{

		testAnAlgorithm(new AdvancedAlgorithm());
	}

	public void testBasicAlgorithm() throws TimeoutException, ResultNotRetrievableException{
		testAnAlgorithm(new BasicAlgorithm());
	}
	
	public void testAdvancedFast() throws TimeoutException, ResultNotRetrievableException{
		testAnAlgorithm(new FasterAdvancedAlgorithm());
	}

	public void testAnAlgorithm(AbstractAlgorithm alg) throws TimeoutException, ResultNotRetrievableException{
		File testFolder = new File("smallCNF");

		for (File f: testFolder.listFiles()){
			System.out.println(f.getPath());
			CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(f.getPath());
			AlgorithmData data = new AlgorithmData(instance);
			
			assertEquals(true, testIfMuse(alg.findAMuse(data)));
		}
	}

	public boolean testIfMuse(CnfSatInstance instance) throws TimeoutException, ResultNotRetrievableException{
		File freezeFile = new File("freeze.tmp");
		File instanceFile = new File("output.tmp.cnf");
		if(freezeFile.exists()){
			freezeFile.delete();
		}
		if(instanceFile.exists()){
			instanceFile.delete();
		}

		int[] freeze = new int[instance.getSize()];
		Arrays.fill(freeze, FreezeFile.UNFREEZE);
		
		DimacsCnfOutput.writeDimacsCnfFile("MUS2.cnf", instance);

		MUCStatistics stat = new MUCStatistics();
		stat.instanceName = "MUS2.cnf";
		MUCExtension.extendCNFBySelVars(new File("MUS2.cnf"), instanceFile, stat); 

		FreezeFile.createFreezeFile(freeze, freezeFile, instance.getHighestVar()+1);

		
		//test if MUS is unsat
		if (solve(freezeFile, instanceFile)){
			System.out.println("is not unsat");
			return false;
		}
		//for each Clause test if it can be removed.
		//TODO
		for (int i = 0; i < freeze.length; i++){
			System.out.println("Clause " + (i+1) + " tested");
			if (i == 0){
				freeze[0] = FreezeFile.FREEZE;
			}else{
				freeze[i-1] = FreezeFile.UNFREEZE;
				freeze[i] = FreezeFile.FREEZE;
			}
			FreezeFile.createFreezeFile(freeze, freezeFile, instance.getHighestVar()+1);
			if (!solve(freezeFile, instanceFile)){
				return false;
			}
		}

		return true;
	}


	protected boolean solve(File freezeFile, File instanceFile) throws TimeoutException, ResultNotRetrievableException{
		File resultFile = new File("result.tmp");
		MiniSAT.solve(instanceFile, new File("proof.tmp") , resultFile , freezeFile);
		return !MiniSAT.wasUnsatisfiable(resultFile);
	}
}
