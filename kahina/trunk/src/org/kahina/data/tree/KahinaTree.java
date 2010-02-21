package org.kahina.data.tree;

import java.util.List;
import java.util.Map;

import org.kahina.core.data.KahinaObject;

public abstract class KahinaTree extends KahinaObject
{
	private static int nextID = 0;
	
	protected LayerDecider decider;

	private KahinaTree primaryModel;

	public KahinaTree(LayerDecider decider)
	{
		super(nextID++);
		this.decider = decider;
		clear();
	}

	/**
	 * Clears the tree model. Implementors should override this method to clear
	 * implementation-specific data, but make sure to call this implementation
	 * using <code>super.clear()</code>.
	 */
	public void clear()
	{
		setPrimaryModel(this);
	}

	public abstract int addNode(String caption, String label, int nodeStatus);

	public abstract boolean hasCollapsedAncestor(int nodeID);

	public abstract void toggleCollapse(int nodeID);

	public abstract void decollapseAll();

	public abstract void decollapse(int nodeID);

	public abstract void collapse(int nodeID);

	public abstract boolean isCollapsed(int nodeID);

	public abstract List<Integer> getLeaves();

	public abstract List<Integer> getChildren(int nodeID, int layerID);

	public abstract int getNodeStatus(int nodeID);

	public abstract String getEdgeLabel(int nodeID);

	public abstract String getNodeCaption(int nodeID);

	public abstract int getParent(int nodeID, int layerID);

	public abstract void addChild(int parent, int child);

	public abstract void setRootID(int rootID);
	
	public abstract int getRootID();

	public abstract int getRootID(int layerID);
	
	public abstract int getSize();
    
    public String exportXML()
    {
        StringBuilder b = new StringBuilder("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<kahinaTree>\n");
        if (getRootID() != -1)
        {
            exportXML(b, getRootID(), 2);
        }
        b.append("</kahinaTree>\n");
        return b.toString();
    }
    
    private void exportXML(StringBuilder b, int node, int depth)
    {
        createSpace(b, depth);
        b.append("<node id=\"" + node + "\" caption=\"" + getNodeCaption(node) + "\" label=\"" + getEdgeLabel(node) + "\" status=\"" + getNodeStatus(node) + "\">\n");
        for (int child : getChildren(node, 0))
        {
            exportXML(b, child, depth + 2);
        }
        createSpace(b, depth);
        b.append("</node>\n");
    }
    
    private void createSpace(StringBuilder b, int amount)
    {
        for (int i = 0; i < amount; i++)
        {
            b.append(' ');
        }
    }

	public KahinaTree getPrimaryModel()
	{
		return primaryModel;
	}

	public void setPrimaryModel(KahinaTree primaryModel)
	{
	    this.primaryModel = primaryModel;
	}

}