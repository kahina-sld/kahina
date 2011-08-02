package org.kahina.core.visual.tree;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListModel;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaListTreeViewPanel extends KahinaViewPanel<KahinaListTreeView> implements MouseListener
{
	private static final long serialVersionUID = -2816651065876855228L;

	private static final boolean VERBOSE = false;

	private JPanel[] panels;
	private JList[] lists;
	private DefaultListModel[] listModels;

	// internal storage for indentations in different layers
	private List<HashMap<Integer, Integer>> indentations;
	// internal storage for the number of primary child choices
    private HashMap<Integer, Integer> numPrimaryAlternatives;
    private HashMap<Integer, Integer> choiceParent;

	// GUI component handling
	private MouseEvent lastMouseEvent;
	private List<JSplitPane> splitPanes;

	public KahinaListTreeViewPanel(int layers, KahinaController control)
	{
		if (VERBOSE)
		{
			System.err.println("new KahinaListTreeViewPanel(" + layers + ")");
		}
		panels = new JPanel[layers];
		lists = new JList[layers];
		listModels = new DefaultListModel[layers];
		clearIndentations();
        clearAlternatives();
		lastMouseEvent = null;
		splitPanes = new LinkedList<JSplitPane>();
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
			// splitPane.setEnabled(false);
			splitPanes.add(splitPane);
			add(splitPane);
		} else
		{
			add(createPane(panels[0]));
		}
		updateDividerLocations();
	}

	private void clearIndentations()
	{
		indentations = new ArrayList<HashMap<Integer, Integer>>();
		for (int i = 0; i < lists.length; i++)
		{
			indentations.add(new HashMap<Integer, Integer>());
		}
	}
    
    private void clearAlternatives()
    {
        numPrimaryAlternatives = new HashMap<Integer, Integer>();
        choiceParent = new HashMap<Integer, Integer>();
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
		clearIndentations();
		for (int i = 0; i < panels.length; i++)
		{
			int rootID = view.secondaryTreeModel.getRootID(i);
			listModels[i] = new DefaultListModel();
			fillListModel(i, rootID, 0);
			lists[i].setModel(listModels[i]);
		}
		for (JPanel panel : panels)
		{
			panel.repaint();
			panel.revalidate();
		}
	}

	private void fillListModel(int layer, int nodeID, int recursionDepth)
	{
		indentations.get(layer).put(nodeID, recursionDepth);
		listModels[layer].addElement(nodeID);
        List<Integer> primaryChildren = view.getVisibleVirtualChildren(view.getTreeModel(), nodeID, layer);
        if (primaryChildren.size() > 1)
        {
            Integer choice = view.primaryChildChoices.get(nodeID);
            if (choice == null)
            {
                choice = 0;
                view.primaryChildChoices.put(nodeID,choice);
            }
            int displayChild = primaryChildren.get(choice);
            choiceParent.put(displayChild, nodeID);
            numPrimaryAlternatives.put(displayChild, primaryChildren.size());
            for (int childID : view.getVisibleVirtualChildren(view.secondaryTreeModel, nodeID, layer))
            {
                int parentID = childID;
                while (parentID != nodeID && parentID != displayChild)
                {
                    parentID = view.getTreeModel().getParent(parentID,layer);
                }
                if (parentID == displayChild)
                {
                    fillListModel(layer, childID, recursionDepth + 1);
                }
            }
        }
        else
        {
    		for (int childID : view.getVisibleVirtualChildren(view.secondaryTreeModel, nodeID, layer))
    		{
                numPrimaryAlternatives.put(childID, 1);
    			fillListModel(layer, childID, recursionDepth + 1);
    		}
        }
	}
    
    public int getChoiceParent(int nodeID)
    {
        return choiceParent.get(nodeID);
    }
    
    public int getNumberOfPrimaryAlternatives(int nodeID)
    {
        Integer result = numPrimaryAlternatives.get(nodeID);
        if (result == null) result = 1;
        return result;
    }
    
    public int getIndentationDepth(int layer, int nodeID)
    {
        Integer depth = indentations.get(layer).get(nodeID);
        if (depth == null) depth = 0;
        return depth;
    }

	public String getIndentingWhitespace(int layer, int nodeID)
	{
		return whitespace(4 * indentations.get(layer).get(nodeID));
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
        //distanceToLeft -= indentations.get(layer).get(clickedNode) * 15;
        if (distanceToLeft < 15)
        {
            return true;
        }
        return false;
    }
    
    private boolean isRightButtonPosition(Point p, JList list, int clickedNode, int layer)
    {
        int distanceToLeft = p.x - list.getComponentAt(p).getX();
        //distanceToLeft -= indentations.get(layer).get(clickedNode) * 15;
        if (distanceToLeft >= 15 && distanceToLeft <= 30)
        {
            return true;
        }
        return false;
    }

	@Override
	public void mouseClicked(MouseEvent e)
	{
		JList list = (JList) e.getComponent();
        int layer = ((KahinaListTreeListRenderer) list.getCellRenderer()).layer;
		int clickedIndex = list.locationToIndex(e.getPoint());
		ListModel model = list.getModel();
		Object element = model.getElementAt(clickedIndex);
		if (element != null)
		{
			int clickedNode;
			clickedNode = (Integer) element;

            if (getNumberOfPrimaryAlternatives(clickedNode)  > 1)
            {
                //UGLY HACK, FORTUNATELY RELIABLE AND NOT EXPENSIVE
                //emulate the check whether one of the "buttons" was clicked
                int choiceParent = getChoiceParent(clickedNode);
                int choice = view.primaryChildChoices.get(choiceParent);
                if (isLeftButtonPosition(e.getPoint(), list, clickedNode, layer))
                {
                    if (choice > 0)
                    {
                        view.primaryChildChoices.put(choiceParent, choice - 1);
                        int selectionNode = view.getVisibleVirtualChildren(view.getTreeModel(), choiceParent, layer).get(choice - 1);
                        KahinaRunner.processEvent(new KahinaSelectionEvent(selectionNode));
                    }
                    return;
                }
                else if (isRightButtonPosition(e.getPoint(), list, clickedNode, layer))
                {
                    if (choice < getNumberOfPrimaryAlternatives(clickedNode) - 1)
                    {
                        view.primaryChildChoices.put(choiceParent, choice + 1);
                        int selectionNode = view.getVisibleVirtualChildren(view.getTreeModel(), choiceParent, layer).get(choice + 1);
                        KahinaRunner.processEvent(new KahinaSelectionEvent(selectionNode));
                    }
                    return;
                }
            }
            if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < 500)
			{
				view.secondaryTreeModel.toggleCollapse(clickedNode);
				updateDisplayAndRepaintFromEventDispatchThread();
				repaint();
			} 
            else
			{
				KahinaRunner.processEvent(new KahinaSelectionEvent(clickedNode));
				lastMouseEvent = e;
			}
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
