package org.kahina.logic.sat.insertionmus.algorithms.MaarenWieringa;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.algorithms.AbstractAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.AlgorithmData;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.MiniSAT;

/**
 * Implementation of H.van Maaren and S. Wieringa
 * "Finding guaranteed MUSes fast"
 * 
 * @author Seitz
 * 
 */
public class AdvancedAlgorithm extends AbstractAlgorithm{

//	protected CnfSatInstance instance; // The instance
//
//	protected ConcurrentSkipListSet<Integer> instanceIDs = new ConcurrentSkipListSet<Integer>();
//	protected TreeSet<Integer> M = new TreeSet<Integer>();// will become the MUS
//	protected ConcurrentSkipListSet<Integer> S = new ConcurrentSkipListSet<Integer>(); // A subset of the
//	// instance, it is
//	// the subset
//	// currently looked
//	// at.
//
//	protected int[] freeze; // variables that should be freezed are marked with
	// 1;

	//	static String path = "../cnf/aim-100-1_6-no-4.cnf";
	//	static String path = "smallCNF/aim-50-1_6-no-1.cnf";
	static String path = "smallCNF/aim-200-2_0-no-1.cnf";
	// static String path = "../cnf/examples/barrel2.cnf";
	// static String path = "../cnf/examples/C168_FW_SZ_66.cnf";
	// static String path = "../cnf/aim-50-2_0-no-2.cnf";
	// static String path = "../cnf/examples/queueinvar4.cnf";

	protected boolean finished = false;

	File instanceFile;

//	public AdvancedAlgorithm(CnfSatInstance instance) {
//		this.instance = instance;
//		for (int i = 0; i < instance.getSize(); i++) {
//			instanceIDs.add(i);
//		}
//		// M.
//
//		MUCStatistics stat = new MUCStatistics();
//		stat.instanceName = path;
//
//		MUCExtension.extendCNFBySelVars(new File(path), new File("output.cnf"),
//				stat);
//
//		this.instanceFile = new File("output.cnf");
//
//		freeze = new int[this.instance.getSize()];
//		Arrays.fill(freeze, FreezeFile.FREEZE);
//	}

	public AdvancedAlgorithm() {
		// TODO Auto-generated constructor stub
	}

	public void run(AlgorithmData data) throws TimeoutException, InterruptedException {
		File freezeFile = new File("freeze" + Thread.currentThread().getId()
				+ ".fr");
		File resultFile = new File("result");

		while (!data.instanceIDs.isEmpty()) {
			data.S = new ConcurrentSkipListSet<Integer>();
			int clauseIDCandidat = -1;
			// int[] oldFreeze = this.freeze.clone();
			Arrays.fill(data.freeze, FreezeFile.FREEZE);
			for (int id : data.M) {
				data.freeze[id] = FreezeFile.UNFREEZE;
			}

			for (int clauseID : data.instanceIDs) {
				List<Integer> clause = data.instance.getClauseByID(clauseID);

				FreezeFile.createFreezeFile(data.freeze, freezeFile,
						data.instance.getHighestVar() + 1, clause);

				MiniSAT.solve(this.instanceFile, resultFile, freezeFile);
				if (!MiniSAT.wasUnsatisfiable(resultFile)) {
					// This Variable may be added
					data.S.add(clauseID);
					// if (clauseIDCandidat == -1)
					clauseIDCandidat = clauseID;
					data.freeze[clauseID] = FreezeFile.UNFREEZE;
					data.S.add(clauseID);
				} else {
					// Never use this variable
				}
				// TODO don't take the last one but let the user choose one.
				// TODO else case, save what is needed from the trace, when
				// searching for more MUSes use it to
				// reduce the Solver-calls
			}
			if (clauseIDCandidat != -1){
				data.M.add(clauseIDCandidat);
				data.S.remove(clauseIDCandidat);
			}
			data.instanceIDs = data.S;
		}
	}


	public static void main(String[] arg0) throws TimeoutException,
	InterruptedException, IOException {

		CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(path);

		AlgorithmData data = new AlgorithmData(instance);
		AdvancedAlgorithm alg = new AdvancedAlgorithm();

		alg.run(data);
		System.out.println("Found a MUS");
		// DimacsCnfOutput.writeDimacsCnfFile("MUS.tmp.cnf", alg.getMUS());

		ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
		for (int i : data.M) {
			System.out.println(i);
			clauseIDs.add(i + 1);
		}
		DimacsCnfOutput.writeDimacsCnfFile("MUS.cnf",
				instance.selectClauses(clauseIDs));
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
		}
		System.out.println("Found a MUS");
		//		DimacsCnfOutput.writeDimacsCnfFile("MUS.tmp.cnf", alg.getMUS());

		ArrayList<Integer> clauseIDs = new ArrayList<Integer>();
		for (int i :data.M){
			//			System.out.println(i);
			clauseIDs.add(i + 1);
			if (i <0){
				System.err.println(data.M);
			}
		}
		return data.instance.selectClauses(clauseIDs);
	}

	@Override
	public boolean nextStep(int clauseIndex, AlgorithmData data) {
		// TODO Auto-generated method stub
		return false;
	}



}
