package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.minisat.MiniSAT;

public class AbstractAlgorithm {

	File resultFile = new File("result");
	File proofFile = new File("proof");
	
	protected boolean solve(CnfSatInstance instance, File freezeFile, File instanceFile) throws TimeoutException, InterruptedException{
		MiniSAT.solve(instanceFile, proofFile , resultFile, freezeFile);
		return !MiniSAT.wasUnsatisfiable(resultFile);
	}
}
