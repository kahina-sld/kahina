package org.kahina.logic.sat.insertionmus;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSAT;
/**
 * Implementation of the insertion algorithm of J.K. de Siqueira N. and J.-F. Puget 
 * "Explanation-based generalisation of failures"
 * @author Seitz
 *
 */
public class BasicAlgorithm {

	private CnfSatInstance instance; // The instance
	private CnfSatInstance M; // will become the MUS
	private CnfSatInstance S; // A subset of the instance, it is the subset corrently looked at.
	private CnfSatInstance unifiedMS;
	private boolean finished = false;

	public BasicAlgorithm(CnfSatInstance instance){
		this.instance = instance;

		this.M = new CnfSatInstance();
		this.S = new CnfSatInstance();
		unifiedMS = new CnfSatInstance();
	}
	/**
	 * 	is the MUS calculated?
	 * @return true if there is a MUS calculated
	 */
	public boolean isFinished(){
		return finished;
	}
	/**
	 * precondition: isFinished() == true
	 * @return one calculated MUS
	 */
	public CnfSatInstance getMUS(){
		return M;
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
	public boolean selectNext(int clauseIndex) throws TimeoutException, InterruptedException, IOException{

//		System.out.println(instance.printAllClauses());
		List<Integer> clause = instance.getClause(clauseIndex);
		instance.removeClauseIndex(clauseIndex);
//		System.out.println(clauseIndex + " Clause: " + clause);
		int clauseID = S.addClause(clause);
		unifiedMS.addClause(clause);
		File fCnf = new File("tmp.cnf");
		File fRes = new File("tmp.res");
		//		minisat.isSatisfiable(cnfFile, tmpResultFile)

		DimacsCnfOutput.writeDimacsCnfFile(fCnf.getName(), unifiedMS);

		if (!MiniSAT.isSatisfiable(fCnf, fRes)){
			//if M united S is not SAT then the clause is part of the MUS
			System.out.println("UNSAT");
			M.addClause(clause);
			S.removeClauseID(clauseID);
			//			S.removeClauseIndex(clauseIndex);
			this.instance = S;
			this.S = new CnfSatInstance();
			unifiedMS = M.copy();
		}

		if (instance.getSize() == 0){
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

		while (!alg.selectNext(instance.getAClauseIndex()));
		System.out.println("Found a MUS");
		DimacsCnfOutput.writeDimacsCnfFile("MUS.tmp.cnf", alg.getMUS());
	}
}
