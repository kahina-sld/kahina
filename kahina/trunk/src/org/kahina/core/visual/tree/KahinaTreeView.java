package org.kahina.core.visual.tree;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.KahinaViewConfiguration;

// TODO Second dimension sometimes isn't properly displayed when there is no
// second-dimension root of the nodes in one view. Test case: set goal nodes to
// be cornerstone nodes in a layer decider.

public class KahinaTreeView extends KahinaView<KahinaTree>
{
	public static final boolean VERBOSE = false;
	
	KahinaTreeViewConfiguration config;

	int treeLayer = 0;
	// layered structure for drawing; also used for reverse indexing
	ArrayList<List<Integer>> nodeLevels;
	Set<Integer> allNodes;

	// is usually null; add another model here for two-dimensional display
	KahinaTree secondaryTreeModel;

	private boolean dimensionsSwapped = false;

	// display coordinates for nodes
	private HashMap<Integer, Integer> nodeX;
	private HashMap<Integer, Integer> nodeY;

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

	// allow marking of a single node in the tree
	private int markedNode;

	// hack to allow precalculations from outside any drawing method
	private Graphics2D g;

	// private variables for internal calculations across functions
	private int nodeHeight; // do not implement a setter for this, but change it with font size
	private int totalTreeHeight;
	private int totalTreeWidth;
	private HashMap<Integer, WidthVector> subtreeWidths;
	private ArrayList<Integer> terminalLayer;
	private int maxNodeWidth;

	public KahinaTreeView(KahinaController control)
	{
		super(control);
		model = new KahinaMemTree();
		treeLayer = 0;
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

		markedNode = -1;

		maxNodeWidth = 1;
	}

	public void display(KahinaTree treeModel)
	{
		treeLayer = 0;
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

	public void display(KahinaTree layerModel, int layerID, int referenceNode)
	{
		treeLayer = layerID;
		layerModel.setReferenceNode(referenceNode);
		model = layerModel;
		nodeBorderColor = new HashMap<Integer, Color>();
		recalculate();
	}
	
	public KahinaTreeViewConfiguration getConfig()
	{
		return config;
	}
	
	public boolean isSecondDimensionDisplayed()
	{
		return config.isSecondDimensionDisplayed() && secondaryTreeModel != null;
	}

	public int getDisplayWidth()
	{
		return totalTreeWidth;
	}

	public int getDisplayHeight()
	{
		return totalTreeHeight;
	}

	public Font getNodeFont(int nodeID)
	{
		int status = getContentfulTreeModel().getNodeStatus(nodeID);
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
		int status = getContentfulTreeModel().getNodeStatus(nodeID);
		Color col = statusNodeColorEncoding.get(status);
		// System.err.println("Node " + nodeID + ": status " + status +
		// " ->  color " + col);
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

	public Integer getNodeX(int nodeID)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".getNodeX(" + nodeID + ")");
		}
		return nodeX.get(nodeID);
	}

	public Integer getNodeY(int nodeID)
	{
		if (config.getDisplayOrientation() == KahinaTreeViewOptions.BOTTOM_UP_DISPLAY)
		{
			return totalTreeHeight - nodeY.get(nodeID);
		}
		return nodeY.get(nodeID);
	}

	public int getNodeHeight(int nodeID)
	{
		return nodeHeight;
	}

	public int getTreeLayer()
	{
		return treeLayer;
	}

	public void setTreeLayer(int treeLayer)
	{
		this.treeLayer = treeLayer;
	}

	public void setStatusColorEncoding(int status, Color color)
	{
		statusNodeColorEncoding.put(status, color);
	}

	public void setStatusFontEncoding(int status, Font font)
	{
		statusFontEncoding.put(status, font);
	}

	public int getMarkedNode()
	{
		return markedNode;
	}

	public void setMarkedNode(int markedNode)
	{
		this.markedNode = markedNode;
	}

	public void resetAllStructures()
	{
		statusDisplayed = new HashMap<Integer, Boolean>();

		nodeLevels = new ArrayList<List<Integer>>();
		allNodes = new HashSet<Integer>();

		nodeX = new HashMap<Integer, Integer>();
		nodeY = new HashMap<Integer, Integer>();

		subtreeWidths = new HashMap<Integer, WidthVector>();
	}

	public void calculateCoordinates()
	{
		int fontSize = config.getZoomLevel();
		int verticalDistance = config.getVerticalDistance();
		int horizontalDistance = config.getHorizontalDistance();
		if (VERBOSE)
			System.err.println("BEGIN: Calculate coordinates for layer " + treeLayer);
		// node height determined by font size
		FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
		nodeHeight = fm.getHeight();

		createNodeLayers();
		// System.err.println(showLevels());
		// System.err.println(terminalLayer);

		totalTreeWidth = 50;
		totalTreeHeight = (nodeLevels.size() + 2) * verticalDistance * fontSize + 10;

		if (model.getRootID(treeLayer) != -1)
		{
			if (VERBOSE)
				System.err.println("BEGIN: Calculate subtree widths for layer " + treeLayer);
			// calculate (maximum) subtree width for each node bottom-up
			if (config.getTerminalsPolicy() != KahinaTreeViewOptions.NO_SPECIAL_TREATMENT)
			{
				for (int node : terminalLayer)
				{
					subtreeWidths.put(node, constructTerminalWidthVector());
				}
			}
			for (int i = nodeLevels.size() - 1; i >= 0; i--)
			{
				if (VERBOSE)
					System.err.println("Node level: " + i);
				for (int node : nodeLevels.get(i))
				{
					int nodeLabelWidth = fm.stringWidth(getContentfulTreeModel().getNodeCaption(node));
					// System.err.println("labelWidth(" +
					// getContentfulTreeModel().getNodeCaption(node) + ") = " +
					// nodeLabelWidth);
					if (maxNodeWidth < nodeLabelWidth) maxNodeWidth = nodeLabelWidth;
					ArrayList<Integer> children = getVisibleVirtualChildren(model, node);
					subtreeWidths.put(node, constructWidthVector(children));
					if (VERBOSE)
						System.err.println("  Node:" + node + " VisChildren:" + children + " WidthVector:" + subtreeWidths.get(node));
				}
			}
			if (VERBOSE)
				System.err.println("COMPLETE: Calculate subtree widths for layer " + treeLayer);
			if (VERBOSE)
				System.err.println("maxNodeWidth = " + maxNodeWidth);
			nodeX.put(model.getRootID(treeLayer), subtreeWidths.get(model.getRootID(treeLayer)).maximumLeftDistance() * horizontalDistance * fontSize / 2);
			// Nodes in this view whose secondary parent is not in this view.
			// Will start from there to display secondary tree:
			ArrayList<Integer> indentAgenda = new ArrayList<Integer>();
			for (int i = 0; i < nodeLevels.size(); i++)
			{
				if (VERBOSE)
					System.err.println("Node level: " + i);
				List<Integer> nodes = nodeLevels.get(i);
				int xOffset = 0;
				if (nodes.size() > 0)
					xOffset = subtreeWidths.get(nodes.get(0)).maximumLeftDistance() * horizontalDistance * fontSize / 2;
				// TODO: find out why this does not seem to have any effect
				if (config.getNodePositionPolicy() == KahinaTreeViewOptions.CENTERED_NODES)
				{
					xOffset += maxNodeWidth;
				}
				if (config.getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
				{
					xOffset += maxNodeWidth;
				}
				int parent = -1;
				WidthVector subtreeWidth = new WidthVector();
				WidthVector lastSubtreeWidth;
				for (int node : nodes)
				{
					if (VERBOSE) System.err.print("  Node:" + node);
					lastSubtreeWidth = subtreeWidth;
					subtreeWidth = subtreeWidths.get(node);
					xOffset += WidthVector.computeNecessaryDistance(lastSubtreeWidth, subtreeWidth) * horizontalDistance * fontSize / 2;
					// switch to children of next parent node --> jump in x offset
					int newParent = getVisibleParent(node);
					if (VERBOSE)
						System.err.print(" VisParent:" + newParent);
					if (i > 0 && newParent != parent)
					{
						parent = newParent;
						if (VERBOSE)
							System.err.print(" SubtreeWidths:" + subtreeWidths.get(parent));
						// old variant of xOffset computation
						// xOffset = (int)((nodeX.get(parent) - (subtreeWidths.get(parent).getStart(1) * 0.5 - 0.5) * horizontalDistance * fontSize));
						xOffset = nodeX.get(parent) - subtreeWidths.get(parent).getStart(1) / 2 * horizontalDistance * fontSize / 2;
					}
					if (i > 0)
					{
						nodeX.put(node, xOffset);
					}
					nodeY.put(node, verticalDistance * fontSize * i + fontSize * 3);
					if (VERBOSE)
						System.err.println(" X:" + nodeX.get(node) + " Y:" + nodeY.get(node));
					if (VERBOSE)
					{
						System.err.print("indent from " + node + "?");
					}
					// Fill indent agenda:
					if (isSecondDimensionDisplayed() && !allNodes.contains(getVisibleSecondaryParent(node)))
					{
						if (VERBOSE)
						{
							System.err.print(" yes");
						}
						indentAgenda.add(node);
					}
					if (VERBOSE)
					{
						System.err.println();
					}
				}
				// adapt total tree width to maximum level width (i.e. maximum x
				// position of a node in any level)
				if (nodes.size() > 0)
				{
					int nodeLevelWidth = nodeX.get(nodes.get(nodes.size() - 1)) + horizontalDistance * fontSize;
					if (nodeLevelWidth > totalTreeWidth)
					{
						totalTreeWidth = nodeLevelWidth;
					}
				}
			}
			// position terminals
			nodeLevels.get(nodeLevels.size() - 1).addAll(terminalLayer);
			for (int i : terminalLayer)
			{
				nodeX.put(i, nodeX.get(model.getParent(i, treeLayer)));
				nodeY.put(i, (nodeLevels.size() + 1) * verticalDistance * fontSize);
			}
			// move nodes around according to secondary tree structure and policy
			if (isSecondDimensionDisplayed())
			{
				if (VERBOSE)
					System.err.println("BEGIN: adapt X values to secondary tree model");
				int depth = 0;
				if (VERBOSE)
					System.err.println("\tvirtual root id: " + secondaryTreeModel.getRootID(treeLayer));
				if (VERBOSE)
				{
					System.err.println("Indent agenda: " + indentAgenda);
				}
				while (indentAgenda.size() > 0)
				{
					int s = indentAgenda.size();
					for (int i = 0; i < s; i++)
					{
						int nodeID = indentAgenda.remove(0);
						if (nodeX.get(nodeID) != null) // TODO can we really stop at every invisible node?
						{
							if (config.getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
							{
								nodeX.put(nodeID, nodeX.get(nodeID) - depth * fontSize);
							} 
							else
							{

								nodeX.put(nodeID, nodeX.get(nodeID) + depth * fontSize);
							}
							if (VERBOSE)
								System.err.println("\t Shifting node " + nodeID + " by " + depth);
								// System.err.println(" depth(" + nodeID + ") = " + depth);
							indentAgenda.addAll(secondaryTreeModel.getChildren(nodeID, treeLayer, false));
							if (VERBOSE)
								System.err.println("\t Agenda: " + indentAgenda);
						}
					}
					depth++;
				}
				if (VERBOSE)
					System.err.println("COMPLETE: adapt X values to secondary tree model");
				// need to adapt tree width; otherwise nodes will protrude over
				// the edge of the tree
				totalTreeWidth += depth * fontSize;
			}
		}
		if (VERBOSE)
			System.err.println("COMPLETE: Calculate coordinates for layer " + treeLayer);
	}

	private void createNodeLayers()
	{
		if (VERBOSE)
			System.err.println("BEGIN: Create node layers for layer " + treeLayer);
		terminalLayer = new ArrayList<Integer>();
		int rootID = model.getRootID(treeLayer);
		ArrayList<Integer> rootLevel = new ArrayList<Integer>();
		if (rootID != -1)
		{
			rootLevel.add(rootID);
			nodeLevels.add(rootLevel);
			allNodes.addAll(rootLevel);
			List<Integer> children = getVisibleVirtualChildren(model, model.getRootID(treeLayer));
			while (true)
			{
				if (VERBOSE) System.err.println("children:" + children);
				ArrayList<Integer> grandchildren = new ArrayList<Integer>();
				for (int i = 0; i < children.size(); i++)
				{
					// terminal handling here
					if (config.getTerminalsPolicy() != KahinaTreeViewOptions.NO_SPECIAL_TREATMENT && getVisibleVirtualChildren(model, children.get(i)).isEmpty())
					{
						terminalLayer.add(children.remove(i));
						i--;
					} 
                    else
					{
						grandchildren.addAll(getVisibleVirtualChildren(model, children.get(i)));
					}
				}
				nodeLevels.add(children);
				allNodes.addAll(children);
				children = grandchildren;
				if (grandchildren.size() == 0)
				{
					break;
				}
			}
		}
		if (VERBOSE) System.err.println("COMPLETE: Create node layers for layer " + treeLayer);
		if (VERBOSE) System.err.println("Levels:\n" + showLevels());
	}

	public boolean displaysNode(int nodeID)
	{
		return (nodeX.get(nodeID) != null);
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
		int status = getContentfulTreeModel().getNodeStatus(nodeID);
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

	private int getVisibleParent(int nodeID)
	{
		int parent = model.getParent(nodeID, treeLayer);
		while (!nodeIsVisible(parent))
		{
			parent = model.getParent(parent, treeLayer);
		}
		return parent;
	}

	private int getVisibleSecondaryParent(int nodeID)
	{
		int secondaryParent = secondaryTreeModel.getParent(nodeID, treeLayer);
		while (!nodeIsVisible(secondaryParent))
		{
			secondaryParent = secondaryTreeModel.getParent(secondaryParent, treeLayer);
		}
		return secondaryParent;
	}

	private ArrayList<Integer> getVisibleVirtualChildren(KahinaTree treeModel, int nodeID)
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
    

	private WidthVector constructWidthVector(ArrayList<Integer> children)
	{
		if (children.size() > 0)
		{
			WidthVector sum = subtreeWidths.get(children.get(0)).copy();
			for (int i = 1; i < children.size(); i++)
			{
				sum = WidthVector.adjoin(sum, subtreeWidths.get(children.get(i)));
			}
			sum.start.add(0, 1);
			sum.end.add(0, 1);
			return sum;
		}
		return new WidthVector();
	}

	private WidthVector constructTerminalWidthVector()
	{
		WidthVector widthVector = new WidthVector();
		for (int i = 0; i < nodeLevels.size(); i++)
		{
			widthVector.start.add(1);
			widthVector.end.add(1);
		}
		return widthVector;
	}

	public FontMetrics getFontMetrics(Font f, Stroke s, int fontSize)
	{
		// hack to allow precalculations from outside any drawing method
		if (g == null)
		{
			BufferedImage bufferedImage = new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR_PRE);
			g = bufferedImage.createGraphics();
		}
		g.setFont(new Font(f.getFontName(), f.getStyle(), fontSize));
		g.setStroke(s);
		return g.getFontMetrics();
	}

	public String showLevels()
	{
		String levelString = "";
		for (List<Integer> nodeLevel : nodeLevels)
		{
			levelString += nodeLevel + "\n";
		}
		return levelString;
	}

	public void swapDimensions()
	{
		if (secondaryTreeModel != null)
		{
			dimensionsSwapped = !dimensionsSwapped;
			KahinaTree firstModel = model;
			model = secondaryTreeModel;
			secondaryTreeModel = firstModel;
		}
	}

	// TODO: make this work also with bottom-up display orientation
	public int nodeAtCoordinates(int x, int y)
	{
		int fontSize = config.getZoomLevel();
		boolean bottomUpVariant = false;
		// change display orientation locally to simplify computations
		int displayOrientation = config.getDisplayOrientation();
		if (displayOrientation == KahinaTreeViewOptions.BOTTOM_UP_DISPLAY)
		{
			y = totalTreeHeight - y;
			bottomUpVariant = true;
			displayOrientation = KahinaTreeViewOptions.TOP_DOWN_DISPLAY;
		}
		// binary search over node levels to determine row
		int lowerIndex = 0;
		int upperIndex = nodeLevels.size() - 1;
		int middleIndex = (lowerIndex + upperIndex) / 2;
		int middleBound = nodeY.get(nodeLevels.get(middleIndex).get(0)) + 2;
		if (bottomUpVariant)
			middleBound += getNodeHeight(nodeLevels.get(middleIndex).get(0));
		if (upperIndex != 0) // simply take the only existing level if there is only one
		{
			while (lowerIndex + 1 < upperIndex)
			{
				// System.err.println("lower: " + lowerIndex + " upper: " +
				// upperIndex + " middle bound: " + middleBound + " y: " + y);
				if (middleBound >= y)
				{
					upperIndex = middleIndex;
				} 
				else
				{
					lowerIndex = middleIndex;
				}
				middleIndex = (lowerIndex + upperIndex) / 2;
				middleBound = nodeY.get(nodeLevels.get(middleIndex).get(0)) + 2;
				if (bottomUpVariant) middleBound += getNodeHeight(nodeLevels.get(middleIndex).get(0));
			}
			if (y < middleBound) upperIndex--;
		}
		// System.err.println("Node Level: " + upperIndex);

		// binary search over nodes in
		List<Integer> selectedLevel = nodeLevels.get(upperIndex);
		int candidateNode = -1;
		if (selectedLevel.size() == 1)
		{
			candidateNode = selectedLevel.get(0);
		} 
		else
		{
			lowerIndex = 0;
			upperIndex = selectedLevel.size() - 1;
			middleIndex = (lowerIndex + upperIndex) / 2;
			FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
			int width = fm.stringWidth(getContentfulTreeModel().getNodeCaption(selectedLevel.get(middleIndex)));
			middleBound = nodeX.get(selectedLevel.get(middleIndex)) + width / 2 + 2;
			if (config.getNodePositionPolicy() == KahinaTreeViewOptions.LEFT_ALIGNED_NODES)
			{
				middleBound += width / 2;
			} 
			else if (config.getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
			{
				middleBound -= width / 2;
			}
			// System.err.println("lower: " + lowerIndex + " upper: " +
			// upperIndex + " middle bound: " + middleBound + " x: " + x);
			while (lowerIndex + 1 < upperIndex)
			{
				if (middleBound >= x)
				{
					upperIndex = middleIndex;
				} else
				{
					lowerIndex = middleIndex;
				}
				middleIndex = (lowerIndex + upperIndex) / 2;
				width = fm.stringWidth(getContentfulTreeModel().getNodeCaption(selectedLevel.get(middleIndex)));
				middleBound = nodeX.get(selectedLevel.get(middleIndex)) + width / 2 - 2;
				if (config.getNodePositionPolicy() == KahinaTreeViewOptions.LEFT_ALIGNED_NODES)
				{
					middleBound += width / 2;
				} 
				else if (config.getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
				{
					middleBound -= width / 2;
				}
				// System.err.println("lower: " + lowerIndex + " upper: " +
				// upperIndex + " middle bound: " + middleBound + " x: " + x);
			}
			if (x < middleBound) upperIndex--;
			candidateNode = selectedLevel.get(upperIndex);
		}
		// System.err.println("Potentially clicked node: " + candidateNode);

		// test coordinates against exact boundaries of candidate node
		FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
		int width = fm.stringWidth(getContentfulTreeModel().getNodeCaption(candidateNode));
		int xLeft = getNodeX(candidateNode) - width / 2 - 2;
		if (config.getNodePositionPolicy() == KahinaTreeViewOptions.LEFT_ALIGNED_NODES)
		{
			xLeft += width / 2;
		} 
		else if (config.getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
		{
			xLeft -= width / 2;
		}
		int xRight = xLeft + width + 4;
		int yBottom = getNodeY(candidateNode) + 2;
		int yTop = yBottom - getNodeHeight(candidateNode);
		if (bottomUpVariant)
		{
			yTop = yBottom - 4;
			yBottom = yTop + getNodeHeight(candidateNode);
		}
		// System.err.println("test: " + xLeft + " <= " + x + " <= " + xRight +
		// "; " + yTop + " <= " + y + " <= " + yBottom);
		if (xLeft <= x && x <= xRight && yTop <= y && y <= yBottom)
		{
			// System.err.println("Click on node: " + candidateNode);
		} 
		else
		{
			// System.err.println("No node found!");
			return -1;
		}
		return candidateNode;
	}

	public KahinaTree getTreeModel()
	{
		return model;
	}

	public KahinaTree getContentfulTreeModel()
	{
		if (dimensionsSwapped)
		{
			return secondaryTreeModel;
		} else
		{
			return model;
		}
	}

	@Override
	public JComponent wrapInPanel(KahinaController control)
	{
		KahinaTreeViewPanel panel = new KahinaTreeViewPanel(control);
		control.registerListener("redraw", panel);
		panel.setView(this);
		JScrollPane scrollPane = new JScrollPane(panel);
		scrollPane.getViewport().setBackground(config.getBackgroundColor());
		return scrollPane;
	}

	@Override
	public void recalculate()
	{
		resetAllStructures();
		calculateCoordinates();
		// System.err.println("Levels:\n" + showLevels());
	}

	/**
	 * tell the GUI to adapt horizontal distance in order to avoid clashes will
	 * be overridden by manual distance adaptation
	 */
	public void avoidClashesByAdaptingHorizontalDistance()
	{
		// stepwise increase of horizontal distance until all clashes are
		// avoided
		while (maxNodeWidth > config.getHorizontalDistance() * config.getZoomLevel() / 2)
		{
			config.setHorizontalDistance(config.getHorizontalDistance() + 1);
		}
		calculateCoordinates();
	}
}
