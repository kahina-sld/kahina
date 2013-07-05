package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.io.ResultReader;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;

public class CombinedAlgorithm extends AbstractAlgorithm {

	protected CnfSatInstance mapInstance; // The instance
	protected CnfSatInstance instance; // The instance
	protected TreeSet<Integer> instanceIDs = new TreeSet<Integer>();
	protected TreeSet<Integer> Selection = new TreeSet<Integer>();// the currently selected Clausses


	File instanceFile;
	private AlgorithmData data;
	private AlgorithmData metaData;
	
	protected void growSelection() throws TimeoutException, InterruptedException{
		for (int clause: instanceIDs){
			if (data.freeze[clause] == FreezeFile.FREEZE){
				data.freeze[clause] = FreezeFile.UNFREEZE;
				
				FreezeFile.createFreezeFile(data.freeze, data.freezeFile, instance.getHighestVar()+1);
				
				if (!solve(data)){
					data.freeze[clause] = FreezeFile.FREEZE;
				}
			}
		}
	}

	
	protected void shrink() throws TimeoutException, InterruptedException{
		List<Integer> removeLater = new ArrayList<Integer>();
		for (int clause: Selection){

			data.freeze[clause] = FreezeFile.FREEZE;
			if(!solve(data)){
				removeLater.add(clause);
			}else{
				data.freeze[clause] = FreezeFile.UNFREEZE;
			}
		}
	}
	
	/**
	 * Implementation of the MARCO algorithm
	 * @throws InterruptedException 
	 * @throws TimeoutException 
	 * @throws IOException 
	 */
	public void findAllMUS(AlgorithmData data) throws TimeoutException, InterruptedException, IOException{

		
		while (this.solve(mapInstance, data)){
			getMap(data);
//			FreezeFile.createFreezeFile(data.freeze, freezeFile, this.instance.getHighestVar()+1);
			
			//TODO create instance File within the constructor
//			MiniSAT.solve(this.instanceFile, data.resultFile, freezeFile);
			
			if (!solve(metaData)){
				shrink();
				//TODO MUS speichern
				//TODO Map erweitern
			}else{
				growSelection();
				//TODO Map erweitern
			}
		}
	}

	private boolean solve(CnfSatInstance instance, AlgorithmData data) throws TimeoutException, InterruptedException, IOException {
		File cnfFile = new File("tmp.cnf");
		DimacsCnfOutput.writeDimacsCnfFile(cnfFile.getName(), mapInstance);
		return MiniSAT.isSatisfiable(cnfFile, data.resultFile);
	}

	/*
	 * adjusts the freezed variables to the new map
	 */
	private void getMap(AlgorithmData data) throws IOException {
//		CnfSatInstance map = DimacsCnfParser.parseDimacsCnfFile(result.getName());
		ResultReader.readAssignment(data.freeze, data.resultFile);
	}

	@Override
	public CnfSatInstance findAMuse(AlgorithmData data) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean nextStep(int clauseIndex, AlgorithmData data) {
		// TODO Auto-generated method stub
		return false;
	}

		
}
