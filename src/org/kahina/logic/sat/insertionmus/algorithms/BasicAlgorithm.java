package org.kahina.logic.sat.insertionmus.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;
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

	protected Integer addToMouseID = -1;

	protected boolean finished = false;


	public BasicAlgorithm(CnfSatInstance instance){
		this.newInstance(instance);
	}
	
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
	public boolean selectNext(int clauseID) throws TimeoutException, IOException{
		//M U S are always freezed

		this.data.instanceIDs.remove(clauseID);
		data.S.add(clauseID);

		data.freeze[clauseID] = FreezeFile.UNFREEZE;


		File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");

		FreezeFile.createFreezeFile(data.freeze, freezeFile, data.instance.getHighestVar()+1);
		File resultFile = new File("result");
		MiniSAT.solve(this.data.instanceFile, resultFile, freezeFile);
		freezeFile.delete();

		if (MiniSAT.wasUnsatisfiable(resultFile)){
			//if M united S is not SAT then the clause is part of the MUS
			System.out.println("UNSAT");
			data.M.add(clauseID);
			data.S.remove(clauseID);

			for (int f: data.S){
				data.freeze[f] = FreezeFile.FREEZE;
			}

			this.data.instanceIDs = data.S;
			this.data.S = new ConcurrentSkipListSet<Integer>();
			

			
			FreezeFile.createFreezeFile(data.freeze, freezeFile, data.instance.getHighestVar()+1);
			MiniSAT.solve(this.data.instanceFile, new File("proof") , resultFile, freezeFile);
			freezeFile.delete();
			if (MiniSAT.wasUnsatisfiable(resultFile)){
				this.data.isMus = true;
				return true;
			}
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
			this.data.isMus = true;
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

		BasicAlgorithm alg = new BasicAlgorithm(instance);

		//		TreeSet<Integer> copy = (TreeSet<Integer>) alg.instanceIDs.clone();
		while (!alg.selectNext(alg.data.instanceIDs.pollFirst())){
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
		for (int i = 0; i < alg.data.freeze.length; i++){
			if (alg.data.freeze[i] == FreezeFile.UNFREEZE){
				clauseIDs.add(i+1);
				//				System.out.println(i+1);
			}
		}
		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", instance.selectClauses(clauseIDs));
	}
	@Override
	public CnfSatInstance findAMuse(){
		try {
			while (!selectNext(data.instanceIDs.pollFirst())){
				
			}

			ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
			for (int i = 0; i < data.freeze.length; i++){
				if (data.freeze[i] == FreezeFile.UNFREEZE){
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
	@Override
	public void newInstance(String path) {
		this.data.instance = DimacsCnfParser.parseDimacsCnfFile(path);
		for (int i = 0; i < data.instance.getSize(); i++){
			data.instanceIDs.add(i);
		}

		MUCStatistics stat = new MUCStatistics();
		stat.instanceName = path;

		MUCExtension.extendCNFBySelVars(new File(path), new File("output.cnf"), stat); 

		this.data.instanceFile = new File("output.cnf");

		data.freeze = new int[this.data.instance.getSize()];
		Arrays.fill(data.freeze, FreezeFile.FREEZE);
	}

	@Override
	public boolean nextStep() {
		try {
			return this.selectNext(data.instanceIDs.pollFirst());
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}
}
