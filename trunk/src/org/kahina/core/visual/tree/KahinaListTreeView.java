package org.kahina.core.visual.tree;

import java.awt.Color;
import java.awt.Font;
import java.awt.Stroke;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.event.KahinaUpdateEvent;

public class KahinaListTreeView extends KahinaAbstractTreeView
{
	private static final boolean VERBOSE = false;
	
	KahinaTreeViewConfiguration config;
	
	private final int[] layers;
	KahinaTree secondaryTreeModel;
	
	// special display properties for certain nodes
	HashMap<Integer, Color> nodeBorderColor;
	HashMap<Integer, Boolean> statusDisplayed;

	// mapping from status values to display properties
	HashMap<Integer, Color> statusNodeColorEncoding;
	HashMap<Integer, Color> statusEdgeColorEncoding;
	HashMap<Integer, Color> statusBorderColorEncoding;
	HashMap<Integer, Stroke> statusStrokeEncoding;
	HashMap<Integer, Font> statusFontEncoding;
	HashMap<Integer, Boolean> statusVisibilityEncoding;
    
    // remember which branch of the primary tree is being visualized
    HashMap<Integer, Integer> primaryChildChoices;
	
	// allow marking of trees on different layers
	private int[] markedNodes;
	
	public KahinaListTreeView(KahinaController control, int... layers)
	{
		super(control);
		this.layers = layers;
		model = new KahinaMemTree();
		secondaryTreeModel = null;
		resetAllStructures();
		nodeBorderColor = new HashMap<Integer, Color>();
		statusNodeColorEncoding = new HashMap<Integer, Color>();
		statusEdgeColorEncoding = new HashMap<Integer, Color>();
		statusBorderColorEncoding = new HashMap<Integer, Color>();
		statusStrokeEncoding = new HashMap<Integer, Stroke>();
		statusFontEncoding = new HashMap<Integer, Font>();
		statusVisibilityEncoding = new HashMap<Integer, Boolean>();
		
		config = new KahinaTreeViewConfiguration();

        primaryChildChoices = new HashMap<Integer, Integer>();
		markedNodes = new int[layers.length];
	}	

	public void display(KahinaTree treeModel)
	{
		model = treeModel;
		nodeBorderColor = new HashMap<Integer, Color>();
		if (VERBOSE)
		{
			System.err.println("Recalculating...");
		}
		recalculate(); // TODO is this necessary?
		if (VERBOSE)
		{
			System.err.println("Recalculated.");
		}
	}

	public void displaySecondaryTree(KahinaTree treeModel)
	{
		this.secondaryTreeModel = treeModel;
		(this.secondaryTreeModel).setReferenceNode(model.getReferenceNode());
		this.secondaryTreeModel.setPrimaryModel(model);
		nodeBorderColor = new HashMap<Integer, Color>();
		recalculate(); // TODO is this necessary?
	}
	
	public void display(KahinaTree layerModel, int referenceNode)
	{
		layerModel.setReferenceNode(referenceNode);
		model = layerModel;
		nodeBorderColor = new HashMap<Integer, Color>();
		recalculate();
	}
	
	public KahinaTreeViewConfiguration getConfig()
	{
		return config;
	}
	
	public void setConfig(KahinaTreeViewConfiguration config)
	{
		this.config = config;
	}
	
	public boolean isSecondDimensionDisplayed()
	{
		return config.isSecondDimensionDisplayed() && secondaryTreeModel != null;
	}

	public Font getNodeFont(int nodeID)
	{
		int status = model.getNodeStatus(nodeID);
		Font fnt = statusFontEncoding.get(status);
		if (fnt == null)
		{
			if (model.isCollapsed(nodeID) && config.getCollapsePolicy() == KahinaTreeViewOptions.COLLAPSE_PRIMARY)
			{
				return new Font(Font.SANS_SERIF, Font.BOLD, config.getZoomLevel());
			}
			if (secondaryTreeModel != null && secondaryTreeModel.isCollapsed(nodeID) && config.getCollapsePolicy() == KahinaTreeViewOptions.COLLAPSE_SECONDARY)
			{
				return new Font(Font.SANS_SERIF, Font.BOLD, config.getZoomLevel());
			}
			return new Font(Font.SANS_SERIF, Font.PLAIN, config.getZoomLevel());
		} else
		{
			return new Font(fnt.getFamily(), fnt.getStyle(), config.getZoomLevel());
		}
	}

	public Color getNodeColor(int nodeID)
	{
		int status = model.getNodeStatus(nodeID);
		Color col = statusNodeColorEncoding.get(status);
		if (VERBOSE)
		{
			System.err.println("Node " + nodeID + ": status " + status + " ->  color " + col);
		}
		if (col == null)
		{
			return Color.WHITE;
		} else
		{
			return col;
		}
	}

	public void setNodeBorderColor(int nodeID, Color color)
	{
		if (color == null)
		{
			nodeBorderColor.remove(nodeID);
		} else
		{
			nodeBorderColor.put(nodeID, color);
		}
	}

	public Color getNodeBorderColor(int nodeID)
	{
		return nodeBorderColor.get(nodeID);
	}
	
	/**
	 * Returns the edge style for the edge leading to a node.
	 * At the moment, this is a constant since dotted lines are not yet implemented.
	 * @param nodeID
	 * @return KahinaTreeViewOptions.COMPLETE_LINES
	 */
	public int getEdgeStyle(int nodeID)
	{
		return KahinaTreeViewOptions.COMPLETE_LINES;
	}
	
	@Override
	public void setStatusColorEncoding(int status, Color color)
	{
		if (VERBOSE)
		{
			System.err.println("status " + status + " -> color " + color);
		}
		statusNodeColorEncoding.put(status, color);
	}

	public void setStatusFontEncoding(int status, Font font)
	{
		statusFontEncoding.put(status, font);
	}

	public int getMarkedNode(int layer)
	{
		return markedNodes[layer];
	}

	public void setMarkedNode(int layer, int markedNode)
	{
		markedNodes[layer] = markedNode;
		//System.err.println("setMarkedNode(" + layer + "," + markedNode + ")");
		if (markedNode != -1)
		{	
			int childID = markedNode;
			int parentID = secondaryTreeModel.getParent(markedNode,layer);
			//System.err.println("   compare: " + secondaryTreeModel.getRootID(layer) + "?=" + childID);
			while (parentID != -1 && childID != secondaryTreeModel.getRootID(layer))
			{
				childID = parentID;
				parentID = secondaryTreeModel.getParent(parentID,layer);
			}
//          adapt choices such that the newly marked node is displayed
            parentID = getTreeModel().getParent(markedNode,layer);
            while (parentID != -1 && childID != getTreeModel().getRootID(layer))
            {
                //System.err.println("   parentID: " + parentID);
                if (primaryChildChoices.get(parentID) != null)
                {
                    //System.err.println("     currentChoice: " + primaryChildChoices.get(parentID));
                    List<Integer> primaryChildren = getVisibleVirtualChildren(getTreeModel(), parentID, layer);
                    //System.err.println("     children: " + primaryChildren);
                    primaryChildChoices.put(parentID,primaryChildren.indexOf(childID));
                    //System.err.println("     newChoice: " + primaryChildChoices.get(parentID));
                    childID = parentID;
                    parentID = getTreeModel().getParent(parentID,layer);
                }
                else
                {
                    //System.err.println("     currentChoice: null");
                    childID = parentID;
                    parentID = getTreeModel().getParent(parentID,layer);
                }
            }
		}
	}

	public void resetAllStructures()
	{
		statusDisplayed = new HashMap<Integer, Boolean>();
	}


	@Override
	public JComponent wrapInPanel(KahinaController control)
	{
		KahinaListTreeViewPanel panel = new KahinaListTreeViewPanel(layers.length, control);
		control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	public boolean nodeIsVisible(int nodeID)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".nodeIsVisible(" + nodeID + ")");
		}
		// TODO: process conditional option, allow user-definable decision function
		if (config.getNodeDisplayPolicy() == KahinaTreeViewOptions.ALWAYS) return true;
		if (config.getNodeDisplayPolicy() == KahinaTreeViewOptions.NEVER) return false;
		if (VERBOSE)
		{
			System.err.println("Looking for collapsed ancestor...");
		}
		if (secondaryTreeModel != null && config.getCollapsePolicy() == KahinaTreeViewOptions.COLLAPSE_SECONDARY && secondaryTreeModel.hasCollapsedAncestor(nodeID)) return false;
		int status = model.getNodeStatus(nodeID);
		Boolean decision = statusVisibilityEncoding.get(status);
		if (decision == null)
		{
			// default values decide
			if (config.getNodeDisplayPolicy() == KahinaTreeViewOptions.STATUS_DEFAULT_YES) return true;
			if (config.getNodeDisplayPolicy() == KahinaTreeViewOptions.STATUS_DEFAULT_NO) return false;
		}
		if (VERBOSE)
		{
			System.err.println("//" + this + ".nodeIsVisible(" + nodeID + "): " + decision);
		}
		return decision;
	}

	private int getVisibleParent(int nodeID, int treeLayer)
	{
		int parent = model.getParent(nodeID, treeLayer);
		while (!nodeIsVisible(parent))
		{
			parent = model.getParent(parent, treeLayer);
		}
		return parent;
	}

	private int getVisibleSecondaryParent(int nodeID, int treeLayer)
	{
		int secondaryParent = secondaryTreeModel.getParent(nodeID, treeLayer);
		while (!nodeIsVisible(secondaryParent))
		{
			secondaryParent = secondaryTreeModel.getParent(secondaryParent, treeLayer);
		}
		return secondaryParent;
	}

	public ArrayList<Integer> getVisibleVirtualChildren(KahinaTree treeModel, int nodeID, int treeLayer)
	{
		ArrayList<Integer> descendants = new ArrayList<Integer>();
		// System.err.println("\t Actual children for node " + nodeID + ": " +
		// treeModel.getChildren(nodeID,treeLayer));
		if (treeModel.isCollapsed(nodeID) && config.getCollapsePolicy() == KahinaTreeViewOptions.COLLAPSE_PRIMARY)
			return descendants;
		descendants.addAll(treeModel.getChildren(nodeID, treeLayer, true));
		if (VERBOSE)
		{
			System.err.println("Breadth-first search for visible descendants of " + nodeID + ":");
		}
		for (int i = 0; i < descendants.size(); i++)
		{
			if (VERBOSE)
			{
				System.err.println("i == " + i + ", descendants: " + descendants);
			}
			boolean nodeIsVisible = nodeIsVisible(descendants.get(i));
			if (VERBOSE)
			{
				System.err.print(descendants.get(i));
				System.err.print(nodeIsVisible ? "+" : "-");
			}
			if (!nodeIsVisible)
			{
				List<Integer> x = treeModel.getChildren(descendants.remove(i), treeLayer, true);
				descendants.addAll(x);
				i--;
			}
		}
		if (VERBOSE)
		{
			System.err.println();
			System.err.println("Virtual children for node " + nodeID + ": " + descendants);
		}
		return descendants;
	}
	
	public KahinaTree getTreeModel()
	{
		return model;
	}
	
	public KahinaTree getSecondaryModel()
	{
		return secondaryTreeModel;
	}
	
	@Override
	protected void processEvent(KahinaUpdateEvent e)
	{
		// recalculation is implicitly part of this (via marker)
		selectNode(e.getSelectedStep());
	}
	
	public void selectNode(int nodeID)
	{
		if (nodeID == -1)
	    {
	        for (int i = 0; i < layers.length; i++)
	        {
	        	setMarkedNode(i,-1);
	        }
	    }
	    else
	    {
	        model.setReferenceNode(nodeID);
	        secondaryTreeModel.setReferenceNode(nodeID);
	        for (int i = 0; i < layers.length; i++)
	        {
	        	int equivID = secondaryTreeModel.getBestEquivalent(nodeID, i);
	        	setMarkedNode(i,equivID);
	            //view.scrollToNode(newNodeID);
	        }
	    }
	}
}