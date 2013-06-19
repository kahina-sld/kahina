package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.util.Arrays;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;

public abstract class AbstractAlgorithm {

	public AlgorithmData data = new AlgorithmData();

	protected boolean solve(CnfSatInstance instance, File freezeFile, File instanceFile) throws TimeoutException, InterruptedException{
		MiniSAT.solve(instanceFile, data.proofFile , data.resultFile, freezeFile);
		return !MiniSAT.wasUnsatisfiable(data.resultFile);
	}
	
	public abstract CnfSatInstance findAMuse();
	public abstract boolean nextStep();

	public abstract void newInstance(String path);

	public void newInstance(CnfSatInstance satInstance2) {
		this.data.instance = satInstance2;
		this.data.path = "currentinst"+Thread.currentThread().getId() + ".cnf";
		
//		this.instance = instance;
		for (int i = 0; i < data.instance.getSize(); i++){
			data.instanceIDs.add(i);
		}

		MUCStatistics stat = new MUCStatistics();
		stat.instanceName = "output.cnf";
		

		DimacsCnfOutput.writeDimacsCnfFile(this.data.path, data.instance);

		MUCExtension.extendCNFBySelVars(new File(data.path), new File("output.cnf"), stat); 

		this.data.instanceFile  = new File("output.cnf");

		data.freeze = new int[this.data.instance.getSize()];
		Arrays.fill(data.freeze, FreezeFile.FREEZE);
	}
}
