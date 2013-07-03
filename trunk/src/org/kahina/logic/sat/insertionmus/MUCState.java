package org.kahina.logic.sat.insertionmus;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.KahinaState;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.algorithms.AbstractAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.AlgorithmData;
import org.kahina.logic.sat.insertionmus.algorithms.BasicAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.ISortingHeuristic;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCStepType;
import org.kahina.logic.sat.muc.data.UCReducerList;

public class MUCState extends KahinaState
{
	public static boolean VERBOSE = false;

	CnfSatInstance satInstance;
	//    MUCStatistics stat;
	MiniSATFiles files;

	//    MUCMetaInstance metaInstance;
	//    BlocklessBlockHandler blocklessBlocks;
	//    PartitionBlockHandler partitionBlocks;
	//    RecursiveBlockHandler recursiveBlocks;


	ColoredPathDAG decisionGraph;
	UCReducerList reducers;

	Map<MUCStep,Integer> nodeForStep;

	MUCInstance kahina;

	private AlgorithmData data;

	private ISortingHeuristic heuristic;

	private AbstractAlgorithm algorithm = new BasicAlgorithm();

	public MUCState(MUCInstance kahina)
	{
		super(kahina);
		this.kahina = kahina;
		this.satInstance = null;
		//        this.metaInstance = null;
		//        this.blocklessBlocks = null;
		//        this.partitionBlocks = null;
		//        this.recursiveBlocks = null;
		//        this.stat = null;
		this.files = null;
		this.nodeForStep = new HashMap<MUCStep,Integer>();
	}

	public MUCState(MUCInstance kahina, CnfSatInstance satInstance)
	{
		super(kahina);
		this.kahina = kahina;
		this.satInstance = satInstance;


		//        this.files = files;
		this.nodeForStep = new HashMap<MUCStep,Integer>();
	}

	public void initialize()
	{
		super.initialize();
		System.out.println("init State");
		decisionGraph = new ColoredPathDAG();
		reducers = new UCReducerList();
	}

	public void reset()
	{
		this.satInstance = null;
		//        this.metaInstance = null;
		//        this.blocklessBlocks = null;  
		//        this.partitionBlocks = null;
		//        this.recursiveBlocks = null;
		//        this.stat = null;
		if (this.files != null){
			this.files.deleteTempFiles();
		}
		this.files = null;
		this.nodeForStep.clear();
		initialize();
	}

	public MUCInstance getKahina()
	{
		return kahina;
	}


	public MUCStep getSelectedStep()
	{
		int stepID = getSelectedStepID();
		if (stepID == -1) return null;
		return retrieve(MUCStep.class, stepID);
	}
	@Override
	public synchronized <T extends KahinaObject> T retrieve(Class<T> type, int stepID){
		T ret = super.retrieve(type, stepID);

		if (ret instanceof MUCStep){
			MUCStep step = (MUCStep)ret;
			if (this.heuristic != null)
				step.setHeuristic(heuristic);
		}
		return ret;		
	}



	public void setSatInstance(CnfSatInstance satInstance) {
		this.satInstance = satInstance;
		this.data = new AlgorithmData(satInstance);
		if (this.heuristic != null)
			data.setHeuristic(this.heuristic);
		//		this.data = data;
		//		this.algorithm.setData(data);
	}

	public void setFiles(MiniSATFiles files2) {
		// TODO Auto-generated method stub
		System.out.println("setFiles, TODO");
	}


	//	static int counter = 0;
	public void newStep(MUCStep step, int parrentID){	
		boolean update = false;
		Integer nodeID;
		if ((nodeID = nodeForStep.get(step))== null){ // if it is a new step then do
			nodeID = nextStepID();
			int nodeStatus = step.getData().isMUS()?MUCStepType.MINIMAL:MUCStepType.UNKNOWN;

			decisionGraph.addNode(nodeID, "Init: " + step.getSize() + "", nodeStatus);
			step.setID(nodeID);
			store(nodeID, step);
			nodeForStep.put(step, nodeID);
			//			nodeForStep.
			update = true;
			System.out.println("added " + nodeID + " status: " + nodeStatus);
			if (this.heuristic != null)
				step.setHeuristic(this.heuristic);
		}else{
			System.out.println("NODE ALREADY EXISTED " + nodeID);
			int nodeStatus = step.getData().isMUS()?MUCStepType.MINIMAL:MUCStepType.UNKNOWN;

			if (decisionGraph.getNodeStatus(nodeID) != nodeStatus){
				decisionGraph.setNodeStatus(nodeID, nodeStatus);
				update = true;
			}
		}

		if (parrentID >= 0 && parrentID != nodeID){// if a new edge is needed
			decisionGraph.addEdge(parrentID, nodeID, "");
			update = true;
			System.out.println("added Edge " + parrentID);
		}
		//update view
		if (update){
			kahina.dispatchEvent(new KahinaUpdateEvent(nodeID));
			System.out.println("selectionEvent");
			kahina.dispatchEvent(new KahinaSelectionEvent(nodeID));
		}
		//		System.out.println("finished");
	}

	public CnfSatInstance getSatInstance() {
		return this.satInstance;
	}

	public ColoredPathDAG getDecisionGraph() {
		return this.decisionGraph;
	}

	public boolean stepExists(MUCStep step) {
		return nodeForStep.get(step) != null;
	}

	public void setHeuristic(ISortingHeuristic heuristic) {
		this.heuristic = heuristic;
	}

	public AbstractAlgorithm getAlgorithm() {
		return this.algorithm ;
	}

	public void setAlgorithm(AbstractAlgorithm alg) {
		this.algorithm = alg;
	}
}
