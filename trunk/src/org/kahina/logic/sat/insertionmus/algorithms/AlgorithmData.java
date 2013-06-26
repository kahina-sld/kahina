package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.util.Arrays;
import java.util.concurrent.ConcurrentSkipListSet;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.ISortingHeuristic;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;

public class AlgorithmData {
	public CnfSatInstance instance;
	public ConcurrentSkipListSet<Integer> instanceIDs;
	public ConcurrentSkipListSet<Integer> M;
	private ConcurrentSkipListSet<Integer> S;
	
	
	public ConcurrentSkipListSet<Integer> getS() {
		return S;
	}

	public int[] freeze;
	public File instanceFile;
	public File resultFile;
//	public File proofFile;
	public String path;
	public boolean isMus = false;
	private ISortingHeuristic heuristic;



	public AlgorithmData() {

		instanceIDs = new ConcurrentSkipListSet<Integer>();
		M = new ConcurrentSkipListSet<Integer>();
		S = new ConcurrentSkipListSet<Integer>();
//		S.
		resultFile = new File("result");
//		proofFile = new File("proof");
	}

	public void setHeuristic(ISortingHeuristic heuristic){
		this.heuristic = heuristic;
		ConcurrentSkipListSet<Integer> newInstanceIDs = new ConcurrentSkipListSet<Integer>(heuristic.getComparator());
		newInstanceIDs.addAll(this.instanceIDs);
		this.instanceIDs = newInstanceIDs;
	}
	
	public AlgorithmData(CnfSatInstance satInstance) {
		instanceIDs = new ConcurrentSkipListSet<Integer>();
		M = new ConcurrentSkipListSet<Integer>();
		S = new ConcurrentSkipListSet<Integer>();
//		S.
		resultFile = new File("result");
//		proofFile = new File("proof");
		
		this.instance = satInstance;
		path = "currentinst"+Thread.currentThread().getId() + ".cnf";
		
//		this.instance = instance;
		for (int i = 0; i < instance.getSize(); i++){
			instanceIDs.add(i);
		}

		MUCStatistics stat = new MUCStatistics();
		stat.instanceName = "output.cnf";
		

		DimacsCnfOutput.writeDimacsCnfFile(this.path, instance);

		MUCExtension.extendCNFBySelVars(new File(path), new File("output.cnf"), stat); 

		this.instanceFile  = new File("output.cnf");

		freeze = new int[this.instance.getSize()];
		Arrays.fill(freeze, FreezeFile.FREEZE);
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

	public AlgorithmData clone() {
		AlgorithmData ret = new AlgorithmData();
		
		ret.freeze = this.freeze.clone();
		ret.instance = this.instance;
		ret.instanceFile = this.instanceFile;
		ret.instanceIDs = this.instanceIDs.clone();
		ret.isMus = this.isMus;
		ret.M = this.M.clone();
		ret.S = this.S.clone();
		
		return ret;
	}

	public void resetS() {
		if (heuristic != null)
			this.S = new ConcurrentSkipListSet<Integer>(heuristic.getComparator());
		else
			this.S = new ConcurrentSkipListSet<Integer>();
	}
}