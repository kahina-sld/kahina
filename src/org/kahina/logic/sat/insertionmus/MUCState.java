package org.kahina.logic.sat.insertionmus;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.kahina.core.KahinaState;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.model.CompleteAssignment;
import org.kahina.logic.sat.insertionmus.algorithms.AbstractAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.AlgorithmData;
import org.kahina.logic.sat.insertionmus.algorithms.BasicAlgorithm;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCStepType;
import org.kahina.logic.sat.muc.bridge.MUCInstruction;
import org.kahina.logic.sat.muc.data.BlocklessBlockHandler;
import org.kahina.logic.sat.muc.data.MUCMetaInstance;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.data.Overlap;
import org.kahina.logic.sat.muc.data.PartitionBlockHandler;
import org.kahina.logic.sat.muc.data.RecursiveBlockHandler;
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
    
    AbstractAlgorithm algorithm = new BasicAlgorithm();
    
    ColoredPathDAG decisionGraph;
    UCReducerList reducers;
    
    Map<MUCStep,Integer> nodeForStep;
    
    MUCInstance kahina;
    
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
        algorithm = new BasicAlgorithm();
//        this.metaInstance = null;
//        this.blocklessBlocks = null;  
//        this.partitionBlocks = null;
//        this.recursiveBlocks = null;
//        this.stat = null;
        if (this.files != null){
        	this.files.deleteTempFiles();
        }
        this.files = null;
        this.nodeForStep = new HashMap<MUCStep,Integer>();
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

	public void setSatInstance(CnfSatInstance satInstance) {
		this.satInstance = satInstance;
		AlgorithmData data = new AlgorithmData(satInstance);
		this.algorithm.setData(data);
	}

	public void setFiles(MiniSATFiles files2) {
		// TODO Auto-generated method stub
		System.out.println("setFiles, TODO");
	}

	
	static int counter = 0;
	public void newStep(MUCStep step, int parrentID){		
        decisionGraph.addNode(counter, "Init: " + step.getSize() + "", MUCStepType.UNKNOWN);
        step.setID(counter);
        kahina.dispatchEvent(new KahinaSelectionEvent(counter));
        kahina.dispatchEvent(new KahinaUpdateEvent(counter));
        store(counter, step);
        counter++;
	}

	public CnfSatInstance getSatInstance() {
		return this.satInstance;
	}

	public ColoredPathDAG getDecisionGraph() {
		// TODO Auto-generated method stub
		return this.decisionGraph;
	}
}
