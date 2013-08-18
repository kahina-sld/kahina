package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.ResultNotRetrievableException;
/**
 * Implementation of the insertion algorithm of J.K. de Siqueira N. and J.-F. Puget 
 * "Explanation-based generalisation of failures"
 * @author Seitz
 *
 */
public class BasicAlgorithm extends AbstractAlgorithm{



//		static String path = "../cnf/aim-100-1_6-no-4.cnf";
//	static String path = "../cnf/examples/barrel2.cnf";
	static String path = "smallCNF/aim-50-1_6-no-1.cnf";
//		static String path = "../cnf/examples/C168_FW_SZ_66.cnf";
//		static String path = "../cnf/aim-50-2_0-no-2.cnf";
	//	static String path = "../cnf/examples/queueinvar4.cnf";


	protected boolean finished = false;


//	public BasicAlgorithm(AlgorithmData data){
//		this.data = data;
//	}
	
	public BasicAlgorithm() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * 	is the MUS calculated?
	 * @return true if there is a MUS calculated
	 */
	public boolean isFinished(){
		return finished;
	}


	/**
	 * runs the next step of this algorithm
	 * precondition: this.instance.getSize() > 0
	 * @param clauseIndex the index of the clause that should be handled next.
	 * @return true if an MUS is found.
	 * @throws IOException 
	 * @throws InterruptedException 
	 * @throws TimeoutException 
	 */
	public boolean selectNext(int clauseID, AlgorithmData data) throws TimeoutException, IOException{
		//M U S are always freezed

		data.instanceIDs.remove(clauseID);
		data.getS().add(clauseID);

		data.freezeAll[clauseID] = FreezeFile.UNFREEZE;


//		File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");

//		FreezeFile.createFreezeFile(data.freezeAll, data.freezeFile, data.instance.getHighestVar()+1);
//		File resultFile = new File("result");
//		MiniSAT.solve(data.instanceFile, data.resultFile, freezeFile);
//		freezeFile.delete();

		try {
			if (!solve(data)){
				//if M united S is not SAT then the clause is part of the MUS
				System.out.println("UNSAT");
				data.M.add(clauseID);
				data.getS().remove(clauseID);

				for (int f: data.getS()){
					data.freezeAll[f] = FreezeFile.FREEZE;
				}

				data.instanceIDs = data.getS();
				data.resetS();
				

				
//			FreezeFile.createFreezeFile(data.freezeAll, data.freezeFile, data.instance.getHighestVar()+1);
//			MiniSAT.solve(data.instanceFile, new File("proof") , data.resultFile, data.freezeFile);
//			data.freezeFile.delete();
				if (!solve(data)){
					data.isMus = true;
					return true;
				}
			}
		} catch (ResultNotRetrievableException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
//		else{
//			if (instanceIDs.size() == 0){
//				//if the instance is empty then we are done.
//				return true;
//			}
//		}
		if (data.instanceIDs.size() == 0){
			//if the instance is empty then we are done.
//			freeze[clauseID] = FreezeFile.FREEZE;
			data.isMus = true;
			return true;
		}
		return false;
	}

	/**
	 * Simple testfunction for this algorithm.
	 * @throws IOException 
	 * @throws InterruptedException 
	 * @throws TimeoutException 
	 */
	public static void main(String[] arg0) throws TimeoutException, InterruptedException, IOException{

		CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(path);

		BasicAlgorithm alg = new BasicAlgorithm();
		AlgorithmData data = new AlgorithmData(instance);
//		alg.setData(data);

		//		TreeSet<Integer> copy = (TreeSet<Integer>) alg.instanceIDs.clone();
		while (!alg.selectNext(data.instanceIDs.pollFirst(), data)){
			//			System.out.println(alg.instanceIDs.size());
		};
		System.out.println("Found a MUS");
//		for (int i: alg.freeze){
//			if (i != FreezeFile.UNFREEZE){
//				System.out.println("Why?");
//			}
//		}

		//		DimacsCnfOutput.writeDimacsCnfFile("MUS.tmp.cnf", alg.getMUS());

		ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
		for (int i = 0; i < data.freezeAll.length; i++){
			if (data.freezeAll[i] == FreezeFile.UNFREEZE){
				clauseIDs.add(i+1);
				//				System.out.println(i+1);
			}
		}
		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", instance.selectClauses(clauseIDs));
	}
	
	@Override
	public CnfSatInstance findAMuse(AlgorithmData data){
		try {
			while (!selectNext(data.instanceIDs.pollFirst(), data)){
				
			}

			ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
			for (int i = 0; i < data.freezeAll.length; i++){
				if (data.freezeAll[i] == FreezeFile.UNFREEZE){
					clauseIDs.add(i+1);
					//				System.out.println(i+1);
				}
			}
//			DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", instance.selectClauses(clauseIDs));

			return data.instance.selectClauses(clauseIDs);
			
			
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
//	@Override
//	public void newInstance(String path) {
//		this.data.instance = DimacsCnfParser.parseDimacsCnfFile(path);
//		for (int i = 0; i < data.instance.getSize(); i++){
//			data.instanceIDs.add(i);
//		}
//
//		MUCStatistics stat = new MUCStatistics();
//		stat.instanceName = path;
//
//		MUCExtension.extendCNFBySelVars(new File(path), new File("output.cnf"), stat); 
//
//		this.data.instanceFile = new File("output.cnf");
//
//		data.freeze = new int[this.data.instance.getSize()];
//		Arrays.fill(data.freeze, FreezeFile.FREEZE);
//	}



	@Override
	public boolean nextStep(int clauseIndex, AlgorithmData data){
		data.instanceIDs.remove(clauseIndex);
		boolean ret = data.instanceIDs.size() == 0;
		try {
			this.selectNext(clauseIndex, data);
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
		return ret;
	}
//
//	@Override
//	public boolean nextStep(ISortingHeuristic heuristic, AlgorithmData data) {
//		
//		while (data.S.size() > 1){
//			
//		}
//		return false;
//	}

}
