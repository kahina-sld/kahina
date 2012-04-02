package org.kahina.core.visual.tree;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListModel;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.util.ListUtil;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaListTreeViewPanel extends KahinaViewPanel<KahinaListTreeView> implements MouseListener
{
	private static final long serialVersionUID = -2816651065876855228L;

	private static final boolean VERBOSE = false;
	
	private final KahinaInstance<?, ?, ?> kahina;

	private final JPanel[] panels;
	private final JList[] lists;
	private final ListModel[] listModels;
	private final List<JSplitPane> splitPanes;	
	private final List<Map<Integer, Integer>> listIndexByNodeIDByLayer;

	// GUI component handling
	private MouseEvent lastMouseEvent;

	public KahinaListTreeViewPanel(int layers, KahinaInstance<?, ?, ?> kahina)
	{
		if (VERBOSE)
		{
			System.err.println("new KahinaListTreeViewPanel(" + layers + ")");
		}
		panels = new JPanel[layers];
		lists = new JList[layers];
		listModels = new DefaultListModel[layers];
		lastMouseEvent = null;
		splitPanes = new LinkedList<JSplitPane>();
		this.kahina = kahina;
		listIndexByNodeIDByLayer = new ArrayList<Map<Integer, Integer>>(layers);
		
		for (int layer = 0; layer < layers; layer++)
		{
			listIndexByNodeIDByLayer.add(new HashMap<Integer, Integer>());
		}
		
		this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		for (int i = 0; i < panels.length; i++)
		{
			panels[i] = new JPanel();
			panels[i].setLayout(new GridLayout());
			lists[i] = new JList();
			lists[i].setCellRenderer(new KahinaListTreeListRenderer(this, i));
			lists[i].addMouseListener(this);
			listModels[i] = new DefaultListModel();
			lists[i].setModel(listModels[i]);
			panels[i].add(lists[i]);
		}
		if (layers > 1)
		{
			JSplitPane splitPane = createSplitPane(0);
			splitPane.setDividerSize(2);
			splitPanes.add(splitPane);
			add(splitPane);
		} else
		{
			add(createPane(panels[0]));
		}
		updateDividerLocations();
	}

	private void updateDividerLocations()
	{
		for (JSplitPane splitPane : splitPanes)
		{
			splitPane.setDividerLocation(this.getWidth() / panels.length);
		}
		for (int i = 0; i < panels.length; i++)
		{
			// lists[i].setMinimumSize(new Dimension(this.getWidth() /
			// panels.length, this.getHeight()));
			// lists[i].setPreferredSize(new Dimension(this.getWidth() /
			// panels.length, this.getHeight()));
		}
	}

	private JSplitPane createSplitPane(int index)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".createSplitPane(" + index + ")");
		}
		JComponent left = createPane(panels[index]);
		JComponent right;
		index++;
		if (index + 1 == panels.length)
		{
			right = createPane(panels[index]);
		} else
		{
			right = createSplitPane(index);
			((JSplitPane) right).setDividerSize(2);
			splitPanes.add((JSplitPane) right);
		}
		return new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, left, right);
	}

	private JComponent createPane(JComponent panel)
	{
		JScrollPane result = new JScrollPane(panel);
		result.getVerticalScrollBar().setUnitIncrement(16);
		result.getViewport().setBackground(Color.WHITE);
		return result;
	}

	@Override
	public void updateDisplay()
	{
		updateDividerLocations();
		view.secondaryTreeModel.setReferenceNode(view.getModel().getReferenceNode());
		for (int i = 0; i < panels.length; i++)
		{
			int rootID = view.secondaryTreeModel.getRootID(i);
			listModels[i] = createListModel(i, rootID);
			lists[i].setModel(listModels[i]);
		}
		for (JPanel panel : panels)
		{
			panel.repaint();
			panel.revalidate();
		}
		for (int layer = 0; layer < panels.length; layer++)
		{
			Integer listIndex = listIndexByNodeIDByLayer.get(layer).get(view.getMarkedNode(layer));
			
			if (listIndex != null)
			{
				lists[layer].ensureIndexIsVisible(listIndex);
			}
		}
	}

	private ListModel createListModel(int layer, int root)
	{
		DefaultListModel result = new DefaultListModel();
		KahinaListTreeListEntry farLeftRightEntry = new KahinaListTreeListEntry();
		farLeftRightEntry.far = true;
		result.addElement(farLeftRightEntry);
		
		// Prepare to remember the list position for each node:
		Map<Integer, Integer> listIndexByNodeID = listIndexByNodeIDByLayer.get(layer);
		listIndexByNodeID.clear();
		int listIndex = 1; // not 0 - that is the farLeftRightEntry

		// Step 1: traversal of the secondary tree, filtering nodes according to
		// layer and visibility
		Set<Integer> virtualSecondaryDescendants = new HashSet<Integer>();
		Map<Integer, Integer> indentations = new HashMap<Integer, Integer>();
		fillVirtualSecondaryDescendantList(virtualSecondaryDescendants, indentations, layer, root, 0);

		// Step 2: traversal of the chosen spine of primary tree
		List<Integer> currentLeftAlternatives = new ArrayList<Integer>();
		List<Integer> currentRightAlternatives = new ArrayList<Integer>();
		Integer node = root;
		while (true)
		{
			if (virtualSecondaryDescendants.contains(node))
			{
				// display this node, and any alternatives we have collected
				farLeftRightEntry.farLeftEnabled = farLeftRightEntry.farLeftEnabled || !currentLeftAlternatives.isEmpty();
				farLeftRightEntry.farRightEnabled = farLeftRightEntry.farRightEnabled || !currentRightAlternatives.isEmpty();
				listIndexByNodeID.put(node, listIndex++);
				addNodeToListModel(node, result, indentations.get(node), currentLeftAlternatives, currentRightAlternatives, false);
			}
			List<Integer> children = view.getTreeModel().getChildren(node);
			if (children.isEmpty())
			{
				break;
			}
			int choice = view.getPrimaryChildChoice(node);
			// collect left alternatives (starting with left siblings)
			currentLeftAlternatives.addAll(findAlternatives(children.subList(0, choice), virtualSecondaryDescendants));
			// collect right alternatives (starting with right siblings)
			int numChildren = children.size();
			int firstRightAlternativeIndex = choice + 1;
			if (firstRightAlternativeIndex < numChildren)
			{
				List<Integer> newRightAlternatives = findAlternatives(children.subList(firstRightAlternativeIndex, numChildren), virtualSecondaryDescendants);
				if (VERBOSE)
				{
					System.err.println("New right alternatives at node " + node + ": " + newRightAlternatives);
				}
				currentRightAlternatives.addAll(0, newRightAlternatives);
			}
			node = children.get(choice);
		}

		// Step 3: add ghost if there are any collected alternatives that
		// haven't been displayed
		if (!currentLeftAlternatives.isEmpty() || !currentRightAlternatives.isEmpty())
		{
			// indentation doesn't matter for ghosts, at least for the current
			// way to display them
			farLeftRightEntry.farLeftEnabled = farLeftRightEntry.farLeftEnabled || !currentLeftAlternatives.isEmpty();
			farLeftRightEntry.farRightEnabled = farLeftRightEntry.farRightEnabled || !currentRightAlternatives.isEmpty();
			listIndexByNodeID.put(node, listIndex++);
			addNodeToListModel(node, result, 0, currentLeftAlternatives, currentRightAlternatives, true);
		}

		result.addElement(farLeftRightEntry);
		return result;
	}

	/**
	 * @param nodes
	 *            left or right siblings of the node whose alternatives we're
	 *            looking for
	 * @param virtualSecondaryDescendants
	 * @return
	 */
	private List<Integer> findAlternatives(List<Integer> nodes, Set<Integer> virtualSecondaryDescendants)
	{
		List<Integer> result = new ArrayList<Integer>();
		for (int node : nodes)
		{
			result.addAll(findAlternatives(node, virtualSecondaryDescendants));
		}
		return result;
	}

	/**
	 * @param node
	 *            left or right sibling of the node whose alternatives we're
	 *            looking for
	 * @param virtualSecondaryDescendants
	 * @return
	 */
	private List<Integer> findAlternatives(int node, Set<Integer> virtualSecondaryDescendants)
	{
		List<Integer> result = new ArrayList<Integer>();
		if (!findAlternatives(node, result, virtualSecondaryDescendants))
		{
			result.add(node);
		}
		return result;
	}

	/**
	 * This methods recursively traverses the primary tree.
	 * 
	 * @param node
	 *            any node dominated by a left or right sibling of the node
	 *            whose alternatives we're looking for
	 * @param alternatives
	 * @param virtualSecondaryDescendants
	 * @return Whether one or more alternatives have been found, i.e. whether
	 *         node primarily dominates at least one of the
	 *         virtualSecondaryDescendants.
	 */
	private boolean findAlternatives(int node, List<Integer> alternatives, Set<Integer> virtualSecondaryDescendants)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".findAlternatives(" + node + ", " + alternatives + ", " + virtualSecondaryDescendants + ")");
		}
		if (virtualSecondaryDescendants.contains(node))
		{
			alternatives.add(node);
			return true;
		}
		List<Integer> children = view.getTreeModel().getChildren(node);
		if (VERBOSE)
		{
			System.err.println("Finding alternatives under: " + children);
		}
		boolean[] done = new boolean[children.size()];
		boolean doneAny = false;
		for (int i = 0; i < done.length; i++)
		{
			doneAny = doneAny || (done[i] = findAlternatives(children.get(i), alternatives, virtualSecondaryDescendants));
		}
		if (doneAny)
		{
			// The children which do not dominate any of the
			// virtualSecondaryDescendants are nevertheless added to the list
			// of alternatives, as representatives of their "empty" branches.
			for (int i = 0; i < done.length; i++)
			{
				if (!done[i])
				{
					alternatives.add(children.get(i));
				}
			}
		}
		return doneAny;
	}

	private void addNodeToListModel(int node, DefaultListModel result, int indentation, List<Integer> leftAlternatives, List<Integer> rightAlternatives, boolean ghost)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".addNodeToListModel(" + node + ", model, " + indentation + ", " + leftAlternatives + ", " + rightAlternatives + ", " + ghost + ")");
		}
		KahinaListTreeListEntry entry = new KahinaListTreeListEntry();
		entry.nodeID = node;
		entry.indentation = indentation;
		entry.leftAlternatives = ListUtil.integerListToIntArray(leftAlternatives);
		leftAlternatives.clear();
		entry.rightAlternatives = ListUtil.integerListToIntArray(rightAlternatives);
		if (VERBOSE)
		{
			System.err.println("Discharchged right alternatives at node " + node + ": " + rightAlternatives);
		}
		rightAlternatives.clear();
		entry.ghost = ghost;
		result.addElement(entry);
	}

	private void fillVirtualSecondaryDescendantList(Set<Integer> virtualSecondaryDescendants, Map<Integer, Integer> indentations, int layer, int node, int indentation)
	{
		virtualSecondaryDescendants.add(node);
		indentations.put(node, indentation);
		for (int child : view.getVisibleVirtualChildren(view.secondaryTreeModel, node, layer))
		{
			fillVirtualSecondaryDescendantList(virtualSecondaryDescendants, indentations, layer, child, indentation + 1);
		}
	}

	public String whitespace(int length)
	{
		StringBuilder whitespace = new StringBuilder();
		for (int i = 0; i < length; i++)
		{
			whitespace.append(' ');
		}
		return whitespace.toString();
	}

	private boolean isLeftButtonPosition(Point p, JList list, int clickedNode, int layer)
	{
		int distanceToLeft = p.x - list.getComponentAt(p).getX();
		// distanceToLeft -= indentations.get(layer).get(clickedNode) * 15;
		if (distanceToLeft < 15)
		{
			return true;
		}
		return false;
	}

	private boolean isRightButtonPosition(Point p, JList list, int clickedNode, int layer)
	{
		int distanceToLeft = p.x - list.getComponentAt(p).getX();
		// distanceToLeft -= indentations.get(layer).get(clickedNode) * 15;
		if (distanceToLeft >= 15 && distanceToLeft <= 30)
		{
			return true;
		}
		return false;
	}
	
	private boolean isFarLeftButtonPosition(Point p, JList list)
	{
		int distanceToLeft = p.x - list.getComponentAt(p).getX();
		return distanceToLeft < 25;
	}
	
	private boolean isFarRightButtonPosition(Point p, JList list)
	{
		int distanceToLeft = p.x - list.getComponentAt(p).getX();
		return distanceToLeft >= 25 && distanceToLeft <= 50;
	}

	@Override
	public void mouseClicked(MouseEvent e)
	{
		JList list = (JList) e.getComponent();
		int layer = ((KahinaListTreeListRenderer) list.getCellRenderer()).layer;
		int clickedIndex = list.locationToIndex(e.getPoint());
		ListModel model = list.getModel();
		if (clickedIndex == -1)
		{
			return;
		}
		KahinaListTreeListEntry clickedEntry = (KahinaListTreeListEntry) model.getElementAt(clickedIndex);
		if (clickedEntry.far)
		{
			if (isFarLeftButtonPosition(e.getPoint(), list))
			{
				view.autospineLeft();
			} else if (isFarRightButtonPosition(e.getPoint(), list))
			{
				view.autospineRight();
			}
			return;
		}
		int clickedNode;
		clickedNode = clickedEntry.nodeID;
		boolean hasLeftAlternatives = clickedEntry.leftAlternatives.length > 0;
		boolean hasRightAlternatives = clickedEntry.rightAlternatives.length > 0;
		if (hasLeftAlternatives || hasRightAlternatives)
		{
			// UGLY HACK, FORTUNATELY RELIABLE AND NOT EXPENSIVE
			// emulate the check whether one of the "buttons" was clicked
			if (isLeftButtonPosition(e.getPoint(), list, clickedNode, layer))
			{
				if (hasLeftAlternatives)
				{
					// TODO do this without changing the selection?
					kahina.dispatchEvent(new KahinaSelectionEvent(clickedEntry.leftAlternatives[clickedEntry.leftAlternatives.length - 1]));
				}
				return;
			} else if (isRightButtonPosition(e.getPoint(), list, clickedNode, layer))
			{
				if (hasRightAlternatives)
				{
					// TODO do this without changing the selection?
					kahina.dispatchEvent(new KahinaSelectionEvent(clickedEntry.rightAlternatives[0]));
				}
				return;
			}
		}
		if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < 500)
		{
			view.secondaryTreeModel.toggleCollapse(clickedNode);
			updateDisplayAndRepaintFromEventDispatchThread();
			repaint();
		} else
		{
			kahina.dispatchEvent(new KahinaSelectionEvent(clickedNode));
			lastMouseEvent = e;
		}
	}

	@Override
	public void mousePressed(MouseEvent e)
	{
		maybeShowPopup(e);
	}

	@Override
	public void mouseReleased(MouseEvent e)
	{
		maybeShowPopup(e);
	}

	private void maybeShowPopup(MouseEvent e)
	{
		if (e.isPopupTrigger())
		{
			// KahinaTreeViewContextMenu.getMenu(this,
			// view.view).show(e.getComponent(),e.getX(), e.getY());
		}
	}

	@Override
	public void mouseEntered(MouseEvent arg0)
	{
		// TODO Auto-generated method stub
	}

	@Override
	public void mouseExited(MouseEvent arg0)
	{
		// TODO Auto-generated method stub
	}
}
