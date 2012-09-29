package org.kahina.tralesld;

import java.util.HashMap;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.data.chart.KahinaMemChart;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.tralesld.bridge.AuxiliaryTraleInstance;
import org.kahina.tralesld.data.signature.TraleSLDSignature;

public class TraleSLDState extends LogicProgrammingState
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -6388044339949017641L;

	KahinaChart chart;
	TraleSLDSignature signature;
	AuxiliaryTraleInstance trale;

	//coordination between tree and chart
	HashMap<Integer, Integer> edgeToNode;
	HashMap<Integer, Integer> nodeToEdge;

	public TraleSLDState(TraleSLDInstance kahina, boolean withAuxiliaryInstance)
	{
		super(kahina);
		System.err.println("New TraleSLDState() and empty signature");
		System.err.println("Current heap size: " + Runtime.getRuntime().totalMemory());
		
		if (withAuxiliaryInstance)
		{
		    trale = new AuxiliaryTraleInstance(true);
		    trale.start();
		}
		chart = new KahinaMemChart();
		signature = new TraleSLDSignature();
			
		edgeToNode = new HashMap<Integer, Integer>();
		nodeToEdge = new HashMap<Integer, Integer>();
	}
	
	public void initialize()
	{
		super.initialize();
		//keep signature and auxiliary instance (constant across parses)
		chart = new KahinaMemChart();
		edgeToNode = new HashMap<Integer, Integer>();
		nodeToEdge = new HashMap<Integer, Integer>();
	}

	public KahinaChart getChart()
	{
		return chart;
	}
	
	public TraleSLDSignature getSignature()
	{
		return signature;
	}
	
	public AuxiliaryTraleInstance getTrale()
	{
		return trale;
	}

	public void linkEdgeToNode(int edgeID, int nodeID)
	{
		edgeToNode.put(edgeID, nodeID);
		nodeToEdge.put(nodeID, edgeID);
	}

	public int getNodeForEdge(int edgeID)
	{
		Integer result = edgeToNode.get(edgeID);
		if (result == null)
		{
			return -1;
		}
		return result;
	}

	public int getEdgeForNode(int nodeID)
	{
		Integer result = nodeToEdge.get(nodeID);
		if (result == null)
		{
			return -1;
		}
		return result;
	}
	
    public TraleSLDStep get(int id)
    {
        return retrieve(TraleSLDStep.class, id);
    }
}
