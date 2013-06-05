package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.io.ResultReader;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;



/**
 * Implementation of H.van Maaren and S. Wieringa "Finding guaranteed MUSes fast"
 * 
 * @author Seitz
 *
 */
public class FasterAdvancedAlgorithm {

	protected CnfSatInstance instance; // The instance

	protected TreeSet<Integer> instanceIDs = new TreeSet<Integer>();
	protected TreeSet<Integer> M = new TreeSet<Integer>();// will become the MUS
	protected TreeSet<Integer> S = new TreeSet<Integer>(); // A subset of the instance, it is the subset currently looked at.

	protected int[] freeze; //variables that should be freezed are marked with 1;


//	static String path = "../cnf/aim-100-1_6-no-4.cnf";
//	static String path = "../cnf/examples/barrel2.cnf";
//	static String path = "../cnf/examples/C168_FW_SZ_66.cnf";
//	static String path = "../cnf/aim-50-2_0-no-2.cnf";
	static String path = "../cnf/examples/queueinvar4.cnf";



	protected boolean finished = false;

	File instanceFile;


	public FasterAdvancedAlgorithm(CnfSatInstance instance){
		this.instance = instance;
		for (int i = 0; i < instance.getSize(); i++){
			instanceIDs.add(i);
		}

		MUCStatistics stat = new MUCStatistics();
		stat.instanceName = path;

		MUCExtension.extendCNFBySelVars(new File(path), new File("output.cnf"), stat); 

		this.instanceFile  = new File("output.cnf");

		freeze = new int[this.instance.getSize()];
		Arrays.fill(freeze, FreezeFile.FREEZE);
	}

	public void run() throws TimeoutException, InterruptedException, IOException{
		File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");
		File resultFile = new File("result");
		File proofeFile = new File("proof");
		
		Map<Integer, int[]> allocations = new HashMap<Integer, int[]>();

		while (!instanceIDs.isEmpty()){
			System.out.println("next run");
			allocations.clear();
			S = new TreeSet<Integer>();
			int clauseIDCandidat = -1;
			//			int[] oldFreeze = this.freeze.clone();
			Arrays.fill(freeze, FreezeFile.FREEZE);
			for (int id: M){
				freeze[id] = FreezeFile.UNFREEZE;
			}

			int[] lastSatisfingAllocation = new int[this.instance.getHighestVar()+1];
//			System.out.println(this.instance.getHighestVar());
			
			for(int clauseID: instanceIDs){
				
				List<Integer> clause = this.instance.getClauseByID(clauseID);

				FreezeFile.createFreezeFile(freeze, freezeFile, instance.getHighestVar()+1, clause);

				MiniSAT.solve(this.instanceFile, proofeFile , resultFile, freezeFile);
				if (!MiniSAT.wasUnsatisfiable(resultFile)){
					//test if this clause removes some MUS candidats
					ArrayList<Integer> removeLater = new ArrayList<Integer>();
					for (Entry<Integer, int[]> entry: allocations.entrySet()){
						if (!satisfies(clause, entry.getValue())){
							removeLater.add(entry.getKey());
//							System.out.println("Removed candidat.");
						}
					}
					for (Integer Key: removeLater){
						allocations.remove(Key);
					}
					
					//This Variable may be added
					S.add(clauseID);
//					if (clauseIDCandidat == -1)
					clauseIDCandidat = clauseID;
					this.freeze[clauseID] = FreezeFile.UNFREEZE;
					S.add(clauseID);

					ResultReader.readAssignment(lastSatisfingAllocation, resultFile);
//					for (int i = 0; i < lastSatisfingAllocation.length; i++){
//						System.out.print(lastSatisfingAllocation[i]*i + " ");
//					}
//					System.out.println(clause);
					allocations.put(clauseID, lastSatisfingAllocation.clone());
				}else{
					
				}
				//TODO don't take the last one but let the user choose one.
				//TODO else case, save what is needed from the trace, when searching for more MUSes use it to
				// reduce the Solver-calls
			}
			for (int clauseID: allocations.keySet()){
				M.add(clauseID);
				S.remove(clauseID);
				System.out.println("Found a clause");
			}
			M.add(clauseIDCandidat); 
			S.remove(clauseIDCandidat);
			this.instanceIDs = S;
		}
	}

	//	public void runFaster() throws TimeoutException, InterruptedException{
	//		File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");
	//		File resultFile = new File("result");
	//		File proofeFile = new File("proof");
	//
	//		while (!instanceIDs.isEmpty()){
	//			S.clear();
	////			int clauseIDCandidat = -1;
	//			//			int[] oldFreeze = this.freeze.clone();
	//			Arrays.fill(freeze, FreezeFile.FREEZE);
	//			for (int id: M){
	//				freeze[id] = FreezeFile.UNFREEZE;
	//			}
	//
	//			for(int clauseID: instanceIDs){
	//				List<Integer> clause = this.instance.getClauseByID(clauseID);
	//
	//				FreezeFile.createFreezeFile(freeze, freezeFile, instance.getHighestVar()+1, clause);
	//
	//				MiniSAT.solve(this.instanceFile, proofeFile , resultFile, freezeFile);
	//				if (!MiniSAT.wasUnsatisfiable(resultFile)){
	//					System.out.println("SAT");
	//					this.freeze[clauseID] = FreezeFile.UNFREEZE;
	//					M.add(clauseID);
	//					S.add(clauseID);
	//					break;
	////					clauseIDCandidat = clauseID;
	//				}else{
	//					S.add(clauseID);
	//				}
	//			}
	//			this.instanceIDs.removeAll(S);
	////			M.add(clauseIDCandidat);
	////			S.remove(clauseIDCandidat);
	////			this.instanceIDs = S;
	//		}
	//	}

	private boolean satisfies(List<Integer> clause, int[] allocation) {
//		System.out.println(clause);
		for (int l: clause){
			int posOrNeg = allocation[Math.abs(l)];
			if (posOrNeg == FreezeFile.FREEZE && l < 0 || 
					posOrNeg == FreezeFile.UNFREEZE && l > 0){
				return true;
			}
		}
		return false;
	}

	public static void main(String[] arg0) throws TimeoutException, InterruptedException, IOException{

		CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(path);

		FasterAdvancedAlgorithm alg = new FasterAdvancedAlgorithm(instance);

		alg.run();
		System.out.println("Found a MUS");
		//		DimacsCnfOutput.writeDimacsCnfFile("MUS.tmp.cnf", alg.getMUS());

		ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
		for (int i :alg.M){
			System.out.println(i);
			clauseIDs.add(i + 1);
		}
		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", instance.selectClauses(clauseIDs));
	}


}
