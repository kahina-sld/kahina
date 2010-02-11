package org.kahina.gui.breakpoint;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.breakpoint.TreeAutomaton;
import org.kahina.data.KahinaTypeException;
import org.kahina.data.tree.KahinaLayeredTree;
import org.kahina.visual.tree.KahinaTreeView;
import org.kahina.visual.tree.KahinaTreeViewMarker;
import org.kahina.visual.tree.KahinaTreeViewPanel;

public class BreakpointTestWindow extends JFrame implements ActionListener
{
    KahinaLayeredTree model;   
    List<TreeAutomaton> breakpoints;
    
    KahinaTreeView view;
    
    JPanel mainPanel;
    NodeOperationsPanel nodeOpPanel;
    BreakpointEditorHintPanel hintPanel;
    KahinaTreeViewPanel viewPanel;
    
    public BreakpointTestWindow(List<TreeAutomaton> breakpoints)
    {
        this.setTitle("Kahina Breakpoint Test Environment");
        this.setSize(800,600);
        
        mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.PAGE_AXIS));
        
        model = new KahinaLayeredTree();
        model.addNode("start", "none", 0);
        model.setRootID(0);
        this.breakpoints = breakpoints;
        
        for (TreeAutomaton breakpoint : breakpoints)
        {
            breakpoint.setTree(model);
        }
        
        nodeOpPanel = new NodeOperationsPanel(this);
        mainPanel.add(nodeOpPanel);
        
        hintPanel = new BreakpointEditorHintPanel();
        mainPanel.add(hintPanel);
        
        view = new KahinaTreeView();
        try
        {
            view.display(model);
        }
        catch (KahinaTypeException e)
        {
            System.err.println("KahinaTypeException while displaying tree. This should not happen.");
        }
        
        KahinaTreeViewMarker treeMarker = new KahinaTreeViewMarker(model);
        viewPanel = new KahinaTreeViewPanel(treeMarker);
        JScrollPane viewScrollPane = new JScrollPane(viewPanel);
        
        mainPanel.add(viewScrollPane);
        
        add(mainPanel);
        
        viewPanel.setView(view); 
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("addChild"))
        {
            int parentNode = model.getReferenceNode();
            String caption = (String) JOptionPane.showInputDialog(this,"Node caption:", "New node",JOptionPane.PLAIN_MESSAGE);
            int child = model.addNode(caption, "none", 0);
            System.err.println("new child ID: " + child);
            model.addChild(parentNode, child);
            view.resetAllStructures();
            view.calculateCoordinates();
            viewPanel.updateDisplay();
        }
        else if (s.equals("removeNode"))
        {
            
        }
    }
}
