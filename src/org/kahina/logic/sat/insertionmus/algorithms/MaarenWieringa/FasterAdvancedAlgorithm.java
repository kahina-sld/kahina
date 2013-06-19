package org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.algorithms.AbstractAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.Heuristics.AscendingIndexHeuristic;
import org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.Heuristics.AverageVariableOccourrenceHeuristic;
import org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.Heuristics.ISortingHeuristic;
import org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.Heuristics.InvertAHeuristic;
import org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa.Heuristics.LargeClausesFirstHeuristic;
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
public class FasterAdvancedAlgorithm extends AbstractAlgorithm{

	ISortingHeuristic heuristic ;

	protected CnfSatInstance instance; // The instance

	protected List<Integer> instanceIDs;
	protected TreeSet<Integer> M = new TreeSet<Integer>();// will become the MUS
	//	protected TreeSet<Integer> S = new TreeSet<Integer>(); // A subset of the instance, it is the subset currently looked at.
	protected TreeSet<Integer> S = new TreeSet<Integer>();

	protected int[] freeze; //variables that should be freezed are marked with 1;


	////	static String path = "../cnf/aim-100-1_6-no-4.cnf";
//		static String path = "../cnf/examples/barrel2.cnf";
		static String path = "smallCNF/aim-50-1_6-no-1.cnf";
//							smallCNF/aim-50-1_6-no-1.cnf
//		static String path = "smallCNF/aim-200-2_0-no-1.cnf";

//		static String path = "smallCNF/barrel2.cnf";
	//	static String path = "../cnf/examples/C168_FW_SZ_66.cnf";
	//	static String path = "../cnf/aim-50-2_0-no-2.cnf";
	//	static String path = "../cnf/examples/queueinvar4.cnf";
//	static String path = "smallCNF/aim-100-1_6-no-1.cnf";



//	protected boolean finished = false;

	File instanceFile;


	public FasterAdvancedAlgorithm(String path){
		this.newInstance( path);

	}

	public FasterAdvancedAlgorithm() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void newInstance( String path) {
//		this.path = path;
		this.M = new TreeSet<Integer>();// will become the MUS
		this.S = new TreeSet<Integer>();

		this.instanceIDs = new LinkedList<Integer>();		
		//		this.instanceIDs = new 
		this.instance = DimacsCnfParser.parseDimacsCnfFile(path);
		for (int i = 0; i < instance.getSize(); i++){
			instanceIDs.add(i);
			//			instanceIDs.add(e
		}

//		this.heuristic =  new InvertAHeuristic(new LargeClausesFirstHeuristic(instance));
//		this.heuristic = new AscendingIndexHeuristic();
//		this.heuristic = new InvertAHeuristic(new AscendingIndexHeuristic());
//		this.heuristic = new AverageVariableOccourrenceHeuristic(instance);
//		this.heuristic = new InvertAHeuristic(new AverageVariableOccourrenceHeuristic(instance));
		this.heuristic = new LargeClausesFirstHeuristic(instance);
		java.util.Collections.sort(instanceIDs, this.heuristic.getComparator());


		MUCStatistics stat = new MUCStatistics();
		stat.instanceName = path;

		this.instanceFile  = new File("output.cnf");
		MUCExtension.extendCNFBySelVars(new File(path), instanceFile, stat); 


		freeze = new int[this.instance.getSize()];
		Arrays.fill(freeze, FreezeFile.FREEZE);
	}

	public void run() throws TimeoutException, InterruptedException, IOException{
		File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");
		File resultFile = new File("result");
//		if (freezeFile.exists()){
//			freezeFile.delete();
//		}
//		if (resultFile.exists()){
//			resultFile.delete();
//		}
//		if(proofeFile.exists()){
//			proofeFile.delete();
//		}

		Map<Integer, int[]> allocations = new HashMap<Integer, int[]>();

		while (!instanceIDs.isEmpty()){
			System.out.println("next run");
			allocations.clear();
			int clauseIDCandidat = -1;
			//			int[] oldFreeze = this.freeze.clone();
			Arrays.fill(freeze, FreezeFile.FREEZE);
			for (int id: M){
				freeze[id] = FreezeFile.UNFREEZE;
			}

			//			System.out.println(this.instanceIDs);
			int[] lastSatisfingAllocation = new int[this.instance.getHighestVar()+1];
			//			System.out.println(this.instance.getHighestVar());

			for(int clauseID: instanceIDs){

				List<Integer> clause = this.instance.getClauseByID(clauseID);

				FreezeFile.createFreezeFile(freeze, freezeFile, instance.getHighestVar()+1, clause);

				MiniSAT.solve(this.instanceFile , resultFile, freezeFile);
				
			
				
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
				//				System.out.println("Found a clause");
			}
			if (clauseIDCandidat == -1){

			}else{

//				M.add(clauseIDCandidat); 
//				S.remove(clauseIDCandidat);
			}
			//			System.out.println(this.instanceIDs);
			this.instanceIDs.clear();
			this.instanceIDs.addAll(S);
			java.util.Collections.sort(this.instanceIDs, this.heuristic.getComparator());
			//			S = new LinkedList<Integer>();
			S = new TreeSet<Integer>();
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


//		CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(path);
		//		FasterAdvancedAlgorithm alg = new FasterAdvancedAlgorithm(instance);
		FasterAdvancedAlgorithm alg = new FasterAdvancedAlgorithm();
		alg.newInstance( path);
		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", alg.findAMuse());
		
//		MiniSAT.
//		instance = DimacsCnfParser.parseDimacsCnfFile("smallCNF/aim-100-1_6-no-1.cnf");
//		alg.newInstance("smallCNF/aim-100-1_6-no-1.cnf");
//		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", alg.findAMuse());
	}

	@Override
	public CnfSatInstance findAMuse() {
		try {
			this.run();
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("Found a MUS");
		//		DimacsCnfOutput.writeDimacsCnfFile("MUS.tmp.cnf", alg.getMUS());

		ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
		for (int i :this.M){
			//			System.out.println(i);
			clauseIDs.add(i + 1);
		}
		System.out.println(this.M);
		return instance.selectClauses(clauseIDs);
	}

	@Override
	public boolean nextStep() {
		// TODO Auto-generated method stub
		return false;
	}
}
