package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.ISortingHeuristic;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;

public abstract class AbstractAlgorithm {

//	public AlgorithmData data;

//	private ISortingHeuristic heuristic;

	protected boolean solve(CnfSatInstance instance, File freezeFile, File instanceFile, AlgorithmData data) throws TimeoutException, InterruptedException{
		MiniSAT.solve(instanceFile , data.resultFile, freezeFile);
		return !MiniSAT.wasUnsatisfiable(data.resultFile);
	}
	
	public abstract CnfSatInstance findAMuse(AlgorithmData data);

//	public abstract void newInstance(String path);
	
//	public void setData(AlgorithmData data){
//		this.data = data;
//	}

	
	
//	public abstract boolean nextStep(AlgorithmData data);
	
	

	public boolean nextStep(AlgorithmData data) {
		if (data.instanceIDs.size() == 0) {
			data.isMus = true;
			return true;
		}
		return this.nextStep(data.instanceIDs.pollFirst(), data);
	}
	
	/**
	 * 
	 * @param clauseIndex
	 * @return true if a new Step is reached.
	 */
	public abstract boolean nextStep(int clauseIndex, AlgorithmData data);

}
