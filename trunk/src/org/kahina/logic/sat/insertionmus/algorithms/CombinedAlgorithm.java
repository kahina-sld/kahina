package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.io.ResultReader;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;

public class CombinedAlgorithm extends AbstractAlgorithm {

	protected CnfSatInstance mapInstance; // The instance
	protected CnfSatInstance instance; // The instance
	protected TreeSet<Integer> instanceIDs = new TreeSet<Integer>();
//	protected TreeSet<Integer> Selection = new TreeSet<Integer>();// the currently selected Clausses

	protected int[] freeze;

	File instanceFile;
	
	protected void growSelection() throws TimeoutException, InterruptedException{
		for (int clause: instanceIDs){
			if (freeze[clause] == FreezeFile.FREEZE){
				freeze[clause] = FreezeFile.UNFREEZE;
				

				File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");

				FreezeFile.createFreezeFile(freeze, freezeFile, instance.getHighestVar()+1);
				File resultFile = new File("result");
				MiniSAT.solve(this.instanceFile, new File("proof") , resultFile, freezeFile);
				freezeFile.delete();
				
				if (MiniSAT.wasUnsatisfiable(resultFile)){
					freeze[clause] = FreezeFile.FREEZE;
				}
			}
		}
	}
	
	protected void shrink(){
		//TODO
	}
	/**
	 * Implementation of the MARCO algorithm
	 * @throws InterruptedException 
	 * @throws TimeoutException 
	 * @throws IOException 
	 */
	public void findAllMUS() throws TimeoutException, InterruptedException, IOException{

		File freezeFile = new File("tmp.fr");
		
		while (this.solve(mapInstance)){
			getMap();
			FreezeFile.createFreezeFile(freeze, freezeFile, this.instance.getHighestVar()+1);
			
			//TODO create instance File within the constructor
			MiniSAT.solve(this.instanceFile, this.data.resultFile, freezeFile);
			
			if (MiniSAT.wasUnsatisfiable(data.resultFile)){
				shrink();
				//TODO MUS speichern
				//TODO Map erweitern
			}else{
				growSelection();
				//TODO Map erweitern
			}
		}
	}

	private boolean solve(CnfSatInstance instance) throws TimeoutException, InterruptedException, IOException {
		File cnfFile = new File("tmp.cnf");
		DimacsCnfOutput.writeDimacsCnfFile(cnfFile.getName(), mapInstance);
		return MiniSAT.isSatisfiable(cnfFile, data.resultFile);
	}

	/*
	 * adjusts the freezed variables to the new map
	 */
	private void getMap() throws IOException {
//		CnfSatInstance map = DimacsCnfParser.parseDimacsCnfFile(result.getName());
		ResultReader.readAssignment(this.freeze, data.resultFile);
	}

	@Override
	public CnfSatInstance findAMuse() {
		// TODO Auto-generated method stub
		return null;
	}



	@Override
	public boolean nextStep() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void nextStep(int clauseIndex) {
		// TODO Auto-generated method stub
		
	}
		
}
