package org.kahina.core.data.tree;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.kahina.core.data.KahinaObject;

public abstract class KahinaTree extends KahinaObject
{
    /**
	 * 
	 */
	private static final long serialVersionUID = -7193259910680733711L;
	
	private static final boolean VERBOSE = false;

	// TODO Move layer decider to tree view. Trees having deciders is a legacy
	// of the dark age of KahinaDbTrees.
	protected LayerDecider decider;

    private KahinaTree primaryModel;

    private int referenceNode;

    private int rootID;
    
    protected boolean needsUpdate = true;

    public KahinaTree(LayerDecider decider)
    {
        this.decider = decider;
        doClear();
    }

    /**
     * Clears the tree model. Implementors should override this method to clear
     * implementation-specific data, but make sure to call this implementation
     * using <code>super.clear()</code>.
     */
    public void clear()
    {
        doClear();
    }

    private void doClear()
    {
        primaryModel = this;
        rootID = -1;
        needsUpdate = true;
    }

    public LayerDecider getLayerDecider()
    {
        return decider;
    }

    public void setLayerDecider(LayerDecider decider)
    {
        this.decider = decider;
    }
    
    public abstract void addNode(int id, String caption, String label, int nodeStatus);

    public abstract int addNode(String caption, String label, int nodeStatus);
    
    public abstract boolean removeLeaf(int id);

    public abstract void decollapseAll();

    public abstract void decollapse(int nodeID);

    public abstract void collapse(int nodeID);

    public abstract boolean isCollapsed(int nodeID);

    public List<Integer> getLeaves()
    {
        List<Integer> leaves = new LinkedList<Integer>();
        collectLeaves(getRootID(), leaves);
        return leaves;
    }

    protected abstract void collectLeaves(int nodeID, List<Integer> leaves);

    public abstract List<Integer> getChildren(int nodeID);
    
    public abstract List<Integer> getChildren(int nodeID, int layer, boolean stopAtCornerstones);

    public abstract int getNodeStatus(int nodeID);

    public abstract void setNodeStatus(int nodeID, int status);

    public abstract String getEdgeLabel(int nodeID);

    public abstract void setEdgeLabel(int nodeID, String label);

    public abstract String getNodeCaption(int nodeID);

    public abstract void setNodeCaption(int nodeID, String caption);

    public abstract int getParent(int nodeID, int layer);
    
	public abstract int getBestEquivalent(int nodeID, int layer);
	
	public abstract int getLayer(int nodeID);
	
	public abstract void setLayer(int nodeID, int layer);

    public abstract void addChild(int parent, int child);
    
    public abstract void addChildAddListener(KahinaTreeChildAddListener listener);
    
    public abstract void removeChildAddListener(KahinaTreeChildAddListener listener);

    public abstract int getRootID(int layer);

	public abstract int getRootID(int layerID, int referenceNode);
	
	public abstract Set<Integer> getAllNodeIDs();

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
        for (int child : getChildren(node, 0, true))
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

    public int getReferenceNode()
    {
        return referenceNode;
    }

    public void setReferenceNode(int referenceNode)
    {
        this.referenceNode = referenceNode;
    }

    public abstract int getParent(int nodeID);

    public boolean hasCollapsedAncestor(int nodeID)
    {
        int parent = getParent(nodeID);
        while (parent != -1)
        {
        	if (VERBOSE)
        	{
        		System.err.println("Parent: " + parent);
        	}
            if (isCollapsed(parent))
            {
                return true;
            }
            parent = getParent(parent);
        }
        return false;
    }

    public int getRootID()
    {
        return rootID;
    }

    public void setRootID(int rootID)
    {
        this.rootID = rootID;
    }

    public void toggleCollapse(int nodeID)
    {
        if (!isCollapsed(nodeID))
        {
            collapse(nodeID);
        } else
        {
            decollapse(nodeID);
        }
        needsUpdate = true;
    }
    
    public void announceChange()
    {
        needsUpdate = true;
    }
    
    public boolean needsUpdate()
    {
        if (needsUpdate)
        {
            needsUpdate = false;
            return true;
        }
        else 
        {
            return false;
        }
    }
}
