package org.kahina.logic.sat.insertionmus;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
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
public class AdvancedAlgorithm {

	protected CnfSatInstance instance; // The instance

	protected TreeSet<Integer> instanceIDs = new TreeSet<Integer>();
	protected TreeSet<Integer> M = new TreeSet<Integer>();// will become the MUS
	protected TreeSet<Integer> S = new TreeSet<Integer>(); // A subset of the instance, it is the subset currently looked at.

	protected int[] freeze; //variables that should be freezed are marked with 1;


//	static String path = "../cnf/aim-100-1_6-no-4.cnf";
//static String path = "../cnf/examples/barrel2.cnf";
//	static String path = "../cnf/examples/C168_FW_SZ_66.cnf";
//	static String path = "../cnf/aim-50-2_0-no-2.cnf";
	static String path = "../cnf/examples/queueinvar4.cnf";



	protected boolean finished = false;

	File instanceFile;


	public AdvancedAlgorithm(CnfSatInstance instance){
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

	public void run() throws TimeoutException, InterruptedException{
		File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");
		File resultFile = new File("result");
		File proofeFile = new File("proof");

		while (!instanceIDs.isEmpty()){
			S = new TreeSet<Integer>();
			int clauseIDCandidat = -1;
			//			int[] oldFreeze = this.freeze.clone();
			Arrays.fill(freeze, FreezeFile.FREEZE);
			for (int id: M){
				freeze[id] = FreezeFile.UNFREEZE;
			}

			for(int clauseID: instanceIDs){
				List<Integer> clause = this.instance.getClauseByID(clauseID);

				FreezeFile.createFreezeFile(freeze, freezeFile, instance.getHighestVar()+1, clause);

				MiniSAT.solve(this.instanceFile, proofeFile , resultFile, freezeFile);
				if (!MiniSAT.wasUnsatisfiable(resultFile)){
					//This Variable may be added
					S.add(clauseID);
//					if (clauseIDCandidat == -1)
					clauseIDCandidat = clauseID;
					this.freeze[clauseID] = FreezeFile.UNFREEZE;
					S.add(clauseID);
				}else{
					//Never use this variable
				}
				//TODO don't take the last one but let the user choose one.
				//TODO else case, save what is needed from the trace, when searching for more MUSes use it to
				// reduce the Solver-calls
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

	public static void main(String[] arg0) throws TimeoutException, InterruptedException, IOException{

		CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(path);

		AdvancedAlgorithm alg = new AdvancedAlgorithm(instance);

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
