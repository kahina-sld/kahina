package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentSkipListSet;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class AlgorithmData {
	public CnfSatInstance instance;
	public ConcurrentSkipListSet<Integer> instanceIDs;
	public TreeSet<Integer> M;
	public ConcurrentSkipListSet<Integer> S;
	public int[] freeze;
	public File instanceFile;
	public File resultFile;
	public File proofFile;
	public String path;
	public boolean isMus = false;



	public AlgorithmData() {

		instanceIDs = new ConcurrentSkipListSet<Integer>();
		M = new TreeSet<Integer>();
		S = new ConcurrentSkipListSet<Integer>();
//		S.
		resultFile = new File("result");
		proofFile = new File("proof");
	}

	public boolean isMUS() {
		return isMus ;
	}
	
	@Override
	public boolean equals(Object o){
		if (o instanceof AlgorithmData){
			AlgorithmData algData = (AlgorithmData) o;
			return instanceIDs.equals(algData.instanceIDs) && M.equals(algData.M) && S.equals(algData.S);
		}
		return false;
	}
}