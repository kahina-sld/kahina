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
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;
/**
 * Implementation of the insertion algorithm of J.K. de Siqueira N. and J.-F. Puget 
 * "Explanation-based generalisation of failures"
 * @author Seitz
 *
 */
public class BasicAlgorithm {

	private CnfSatInstance instance; // The instance
	
	private TreeSet<Integer> instanceIDs = new TreeSet<Integer>();
	private TreeSet<Integer> M = new TreeSet<Integer>();// will become the MUS
	private TreeSet<Integer> S = new TreeSet<Integer>(); // A subset of the instance, it is the subset currently looked at.

	private int[] freeze; //variables that should be freezed are marked with 1;
	
//	private CnfSatInstance M; 
//	private CnfSatInstance S;
//	private CnfSatInstance unifiedMS;
	private boolean finished = false;
	
	File instanceFile;

	public BasicAlgorithm(CnfSatInstance instance){
		this.instance = instance;
		for (int i = 0; i < instance.getHighestVar(); i++){
			instanceIDs.add(i);
		}
		
		MUCStatistics stat = new MUCStatistics();
		stat.instanceName = "../cnf/aim-100-1_6-no-1.cnf";

		MUCExtension.extendCNFBySelVars(new File("../cnf/aim-100-1_6-no-1.cnf"), new File("output.cnf"), stat); 
		
//		DimacsCnfOutput.writeDimacsCnfFile("instance.cnf", instance);
		
		this.instanceFile  = new File("output.cnf");

//		this.M = new CnfSatInstance();
//		this.S = new CnfSatInstance();
		freeze = new int[this.instance.getHighestVar()];
		Arrays.fill(freeze, MiniSAT.FREEZE);
//		unifiedMS = new CnfSatInstance();
	}
	/**
	 * 	is the MUS calculated?
	 * @return true if there is a MUS calculated
	 */
	public boolean isFinished(){
		return finished;
	}
//	/**
//	 * precondition: isFinished() == true
//	 * @return one calculated MUS
//	 */
//	public CnfSatInstance getMUS(){
//		return M;
//	}


	/**
	 * runs the next step of this algorithm
	 * precondition: this.instance.getSize() > 0
	 * @param clauseIndex the index of the clause that should be handled next.
	 * @return true if an MUS is found.
	 * @throws IOException 
	 * @throws InterruptedException 
	 * @throws TimeoutException 
	 */
	public boolean selectNext(int clauseID) throws TimeoutException, InterruptedException, IOException{

//		System.out.println(instance.printAllClauses());
//		List<Integer> clause = instance.getClause(clauseIndex);
//		instance.removeClauseIndex(clauseIndex);
//		System.out.println(clauseIndex + " Clause: " + clause);
//		int clauseID = S.addClause(clause);
		S.add(clauseID);
		instanceIDs.remove(clauseID);
		
		freeze[clauseID] = MiniSAT.UNFREEZE;
		
////		unifiedMS.addClause(clause);
//		File fCnf = new File("tmp.cnf");
//		File fRes = new File("tmp.res");
		//		minisat.isSatisfiable(cnfFile, tmpResultFile)

//		DimacsCnfOutput.writeDimacsCnfFile(fCnf.getName(), unifiedMS);
		
		File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");
		
		MiniSAT.createFreezeFile(freeze, freezeFile, instance.getHighestVar()+1);
		File resultFile = new File("result");
		MiniSAT.solve(this.instanceFile, new File("proof") , resultFile, freezeFile);
		
//		if (!MiniSAT.isSatisfiable(fCnf, fRes)){
		if (MiniSAT.wasUnsatisfiable(resultFile)){
			//if M united S is not SAT then the clause is part of the MUS
			System.out.println("UNSAT");
			M.add(clauseID);
			S.remove(clauseID);
			
			for (int f: S){
				freeze[f] = MiniSAT.FREEZE;
			}
//			S.removeClauseID(clauseID);
			this.instanceIDs = S;
			this.S = new TreeSet<Integer>();
		}

		if (instanceIDs.size() == 0){
			//if the instance is empty then we are done.
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
		String path = "../cnf/aim-100-1_6-no-1.cnf";
		CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(path);

		BasicAlgorithm alg = new BasicAlgorithm(instance);

		while (!alg.selectNext(alg.instanceIDs.pollFirst())){
//			System.out.println(alg.instanceIDs.size());
		};
		System.out.println("Found a MUS");
//		DimacsCnfOutput.writeDimacsCnfFile("MUS.tmp.cnf", alg.getMUS());
		
		ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
		for (int i = 0; i < alg.freeze.length; i++){
			if (alg.freeze[i] == MiniSAT.UNFREEZE){
				clauseIDs.add(i+1);
			}
		}
		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", instance.selectClauses(clauseIDs));
	}
}
