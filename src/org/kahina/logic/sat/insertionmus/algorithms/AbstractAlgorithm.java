package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.MUCStep;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.ISortingHeuristic;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.ResultNotRetrievableException;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;

public abstract class AbstractAlgorithm {

//	public AlgorithmData data;

//	private ISortingHeuristic heuristic;

	protected boolean solve(AlgorithmData data) throws TimeoutException, ResultNotRetrievableException{
		FreezeFile.createFreezeFile(data.freezeAll, data.freezeFile,
				data.instance.getHighestVar() + 1);
		MiniSAT.solve(data.instanceFile , data.resultFile, data.freezeFile);
		return !MiniSAT.wasUnsatisfiable(data.resultFile);
	}
	protected boolean solve(AlgorithmData data, int[] freeze) throws TimeoutException, ResultNotRetrievableException{
		FreezeFile.createFreezeFile(freeze, data.freezeFile,
				data.instance.getHighestVar() + 1);
		MiniSAT.solve(data.instanceFile , data.resultFile, data.freezeFile);
		return !MiniSAT.wasUnsatisfiable(data.resultFile);
	}

	protected boolean solve(AlgorithmData data,	List<Integer> clause) throws TimeoutException, ResultNotRetrievableException {
		FreezeFile.createFreezeFile(data.freezeAll, data.freezeFile,
				data.instance.getHighestVar() + 1, clause);

		MiniSAT.solve(data.instanceFile, data.resultFile, data.freezeFile);
		
		return !MiniSAT.wasUnsatisfiable(data.resultFile);
	}
	
	public CnfSatInstance findAMuse(AlgorithmData data) throws ResultNotRetrievableException{

		while (!data.isMus){
//			do{
//				System.out.println("nextStep");
				nextStep(data);
//			}while (data.instanceIDs.size() > 0);
		}
		
		ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
		for (int i :data.M){
			//			System.out.println(i);
			clauseIDs.add(i + 1);
		}
		System.out.println("Found a MUS");
		CnfSatInstance ret = data.instance.selectClauses(clauseIDs);

		DimacsCnfOutput.writeDimacsCnfFile("lastMUS.cnf", ret);
		return ret;
	}

//	public abstract void newInstance(String path);
	
//	public void setData(AlgorithmData data){
//		this.data = data;
//	}

	
	
//	public abstract boolean nextStep(AlgorithmData data);
	
	

	public boolean nextStep(AlgorithmData data) throws ResultNotRetrievableException {
//		if (data.instanceIDs.size() == 0) {
//			data.isMus = true;
//			return true;
//		}
		return this.nextStep(data.instanceIDs.pollFirst(), data);
	}
	
	/**
	 * 
	 * @param clauseIndex
	 * @return true if a new Step is reached.
	 * @throws ResultNotRetrievableException 
	 */
	public abstract boolean nextStep(int clauseIndex, AlgorithmData data) throws ResultNotRetrievableException;

}
