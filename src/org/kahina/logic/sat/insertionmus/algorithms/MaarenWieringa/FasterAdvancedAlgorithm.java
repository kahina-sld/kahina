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
import org.kahina.logic.sat.insertionmus.algorithms.AlgorithmData;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.AscendingIndexHeuristic;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.AverageVariableOccourrenceHeuristic;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.ISortingHeuristic;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.InvertAHeuristic;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.LargeClausesFirstHeuristic;
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

	//	protected CnfSatInstance instance; // The instance
	//
	//	protected List<Integer> instanceIDs;
	//	protected TreeSet<Integer> M = new TreeSet<Integer>();// will become the MUS
	//	//	protected TreeSet<Integer> S = new TreeSet<Integer>(); // A subset of the instance, it is the subset currently looked at.
	//	protected TreeSet<Integer> S = new TreeSet<Integer>();
	//
	//	protected int[] freeze; //variables that should be freezed are marked with 1;


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

	//	File instanceFile;


	//	public FasterAdvancedAlgorithm(String path){
	//		this.newInstance( path);
	//
	//	}

	public FasterAdvancedAlgorithm() {
		// TODO Auto-generated constructor stub
	}

	//	@Override
	//	public void newInstance( String path) {
	////		this.path = path;
	//		this.M = new TreeSet<Integer>();// will become the MUS
	//		this.S = new TreeSet<Integer>();
	//
	//		this.instanceIDs = new LinkedList<Integer>();		
	//		//		this.instanceIDs = new 
	//		this.instance = DimacsCnfParser.parseDimacsCnfFile(path);
	//		for (int i = 0; i < instance.getSize(); i++){
	//			instanceIDs.add(i);
	//			//			instanceIDs.add(e
	//		}
	//
	////		this.heuristic =  new InvertAHeuristic(new LargeClausesFirstHeuristic(instance));
	////		this.heuristic = new AscendingIndexHeuristic();
	////		this.heuristic = new InvertAHeuristic(new AscendingIndexHeuristic());
	////		this.heuristic = new AverageVariableOccourrenceHeuristic(instance);
	////		this.heuristic = new InvertAHeuristic(new AverageVariableOccourrenceHeuristic(instance));
	//		this.heuristic = new LargeClausesFirstHeuristic(instance);
	//		java.util.Collections.sort(instanceIDs, this.heuristic.getComparator());
	//
	//
	//		MUCStatistics stat = new MUCStatistics();
	//		stat.instanceName = path;
	//
	//		this.instanceFile  = new File("output.cnf");
	//		MUCExtension.extendCNFBySelVars(new File(path), instanceFile, stat); 
	//
	//
	//		freeze = new int[this.instance.getSize()];
	//		Arrays.fill(freeze, FreezeFile.FREEZE);
	//	}

	@Override
	public boolean nextStep(int clauseID, AlgorithmData data) {

		if (data.instanceIDs.size() <= 0){
			if (data.getS().isEmpty()){
				data.isMus = true;
				return true;
			}

			for (int cID: data.allocations.keySet()){
				data.M.add(cID);
				data.getS().remove(cID);
				//				System.out.println("Found a clause");
			}

			//			System.out.println(this.instanceIDs);
			data.instanceIDs.clear();
			data.instanceIDs.addAll(data.getS());
			//			java.util.Collections.sort(this.data.instanceIDs, this.heuristic.getComparator());
			//			S = new LinkedList<Integer>();
			data.getS().clear();

			data.allocations.clear();

			Arrays.fill(data.freeze, FreezeFile.FREEZE);
			for (int id: data.M){
				data.freeze[id] = FreezeFile.UNFREEZE;
			}

		}

//		int clauseID = data.instanceIDs.pollFirst();
		data.instanceIDs.remove(clauseID);

		List<Integer> clause = data.instance.getClauseByID(clauseID);

		FreezeFile.createFreezeFile(data.freeze, data.freezeFile, data.instance.getHighestVar()+1, clause);

		try {
			MiniSAT.solve(data.instanceFile , data.resultFile, data.freezeFile);
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		if (!MiniSAT.wasUnsatisfiable(data.resultFile)){
			//test if this clause removes some MUS candidats
			ArrayList<Integer> removeLater = new ArrayList<Integer>();
			for (Entry<Integer, int[]> entry: data.allocations.entrySet()){
				if (!satisfies(clause, entry.getValue())){
					removeLater.add(entry.getKey());
					//							System.out.println("Removed candidat.");
				}
			}
			for (Integer Key: removeLater){
				data.allocations.remove(Key);
			}

			//This Variable may be added
			//					if (clauseIDCandidat == -1)
			//				clauseIDCandidat = clauseID;
			data.freeze[clauseID] = FreezeFile.UNFREEZE;
			data.getS().add(clauseID);

			int[] lastSatisfingAllocation = new int[data.instance.getHighestVar()+1];
			try {
				ResultReader.readAssignment(lastSatisfingAllocation, data.resultFile);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			data.allocations.put(clauseID, lastSatisfingAllocation.clone());
		}

		return false;
	}

	public void run(AlgorithmData data) throws TimeoutException, InterruptedException, IOException{
		File freezeFile = new File("freeze"+ Thread.currentThread().getId() + ".fr");
		File resultFile = new File("result");


		Map<Integer, int[]> allocations = new HashMap<Integer, int[]>();

		while (!data.instanceIDs.isEmpty()){
			System.out.println("next run");
			allocations.clear();
			int clauseIDCandidat = -1;
			//			int[] oldFreeze = this.freeze.clone();
			Arrays.fill(data.freeze, FreezeFile.FREEZE);
			for (int id: data.M){
				data.freeze[id] = FreezeFile.UNFREEZE;
			}

			//			System.out.println(this.instanceIDs);
			//			System.out.println(this.instance.getHighestVar());

			for(int clauseID: data.instanceIDs){

				List<Integer> clause = data.instance.getClauseByID(clauseID);

				FreezeFile.createFreezeFile(data.freeze, freezeFile, data.instance.getHighestVar()+1, clause);

				MiniSAT.solve(data.instanceFile , resultFile, freezeFile);

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
					data.freeze[clauseID] = FreezeFile.UNFREEZE;
					data.getS().add(clauseID);

					int[] lastSatisfingAllocation = new int[data.instance.getHighestVar()+1];
					ResultReader.readAssignment(lastSatisfingAllocation, resultFile);
					allocations.put(clauseID, lastSatisfingAllocation.clone());
				}else{

				}
				//TODO don't take the last one but let the user choose one.
				//TODO else case, save what is needed from the trace, when searching for more MUSes use it to
				// reduce the Solver-calls
			}
			for (int clauseID: allocations.keySet()){
				data.M.add(clauseID);
				data.getS().remove(clauseID);
				//				System.out.println("Found a clause");
			}
			if (clauseIDCandidat == -1){

			}else{

				data.M.add(clauseIDCandidat); 
				data.getS().remove(clauseIDCandidat);
			}
			//			System.out.println(this.instanceIDs);
			data.instanceIDs.clear();
			data.instanceIDs.addAll(data.getS());
			//			java.util.Collections.sort(this.data.instanceIDs, this.heuristic.getComparator());
			//			S = new LinkedList<Integer>();
			data.getS().clear();
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

		CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(path);
		FasterAdvancedAlgorithm alg = new FasterAdvancedAlgorithm();

		AlgorithmData data = new AlgorithmData(instance);
		//		alg.setData(data);


		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", alg.findAMuse(data));

		//		MiniSAT.
		//		instance = DimacsCnfParser.parseDimacsCnfFile("smallCNF/aim-100-1_6-no-1.cnf");
		//		alg.newInstance("smallCNF/aim-100-1_6-no-1.cnf");
		//		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf", alg.findAMuse());
	}

	@Override
	public CnfSatInstance findAMuse(AlgorithmData data) {
		try {
			this.run(data);
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
		for (int i :data.M){
			//			System.out.println(i);
			clauseIDs.add(i + 1);
		}
		System.out.println(data.M);
		return data.instance.selectClauses(clauseIDs);
	}



}
