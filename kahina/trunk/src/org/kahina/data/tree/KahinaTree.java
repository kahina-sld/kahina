package org.kahina.data.tree;

import java.util.List;
import java.util.Map;

import org.kahina.core.data.KahinaObject;

public abstract class KahinaTree extends KahinaObject
{

	public KahinaTree(int id)
	{
		super(id);
	}

	public abstract void clear();

	public abstract int addNode(String caption, String label, int nodeStatus);

	public abstract boolean hasCollapsedAncestor(int nodeID);

	public abstract void toggleCollapse(int nodeID);

	public abstract void decollapseAll();

	public abstract void decollapse(int nodeID);

	public abstract void collapse(int nodeID);

	public abstract boolean isCollapsed(int nodeID);

	public abstract void setStatus(Map<Integer, Integer> status);

	public abstract Map<Integer, Integer> getStatus();

	public abstract void setEdgeLabels(Map<Integer, String> edgeLabels);

	public abstract Map<Integer, String> getEdgeLabels();

	public abstract void setNodeCaptions(Map<Integer, String> nodeCaptions);

	public abstract Map<Integer, String> getNodeCaptions();

	public abstract List<Integer> getLeaves();

	public abstract List<Integer> getChildren(int nodeID, int layerID);

	public abstract int getNodeStatus(int nodeID);

	public abstract String getEdgeLabel(int nodeID);

	public abstract String getNodeCaption(int nodeID);

	public abstract int getParent(int nodeID, int layerID);

	public abstract void addChild(int parent, int child);

	public abstract void setRootID(int rootID);

	public abstract int getRootID(int layerID);

	public abstract void setPrimaryModel(KahinaTree primaryModel);

}