package org.kahina.core.gui.breakpoint;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.breakpoint.patterns.KahinaTreeMatchEvent;
import org.kahina.core.data.breakpoint.patterns.TreeAutomaton;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.edit.breakpoint.BreakpointEditorHintPanel;
import org.kahina.core.edit.breakpoint.NodeOperationsPanel;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewMarker;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;

public class BreakpointTestWindow extends JFrame implements ActionListener, KahinaListener
{

	private static final long serialVersionUID = 5249202782237918220L;
	KahinaTree model;   
    List<TreeAutomaton> breakpoints;
    
    KahinaTreeView view;
    
    JPanel mainPanel;
    TreeGenerationPanel treeGenPanel;
    NodeOperationsPanel nodeOpPanel;
    BreakpointEditorHintPanel hintPanel;
    KahinaTreeViewPanel viewPanel;
    
    Thread growthProcess;
    boolean growthMode;
    
    public BreakpointTestWindow(List<TreeAutomaton> breakpoints, KahinaInstance<?, ?, ?> kahina)
    {
        this.setTitle("Kahina Breakpoint Test Environment");
        this.setSize(800,600);
        
        kahina.getGuiControl().registerListener("treeMatch", this);
        
        mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.PAGE_AXIS));
        
        model = new KahinaMemTree();
        int rootID = model.addNode("start", "none", 0);
        model.setRootID(rootID);
        this.breakpoints = breakpoints;
        
        for (TreeAutomaton breakpoint : breakpoints)
        {
            breakpoint.setTree(model);
            breakpoint.setController(kahina.getGuiControl());
        }
        
        treeGenPanel = new TreeGenerationPanel(this);
        mainPanel.add(treeGenPanel);
        
        nodeOpPanel = new NodeOperationsPanel(this);
        mainPanel.add(nodeOpPanel);
        
        hintPanel = new BreakpointEditorHintPanel();
        mainPanel.add(hintPanel);
        
        view = new KahinaTreeView(kahina);
        view.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
        view.getConfig().setVerticalDistance(4);
        view.getConfig().setHorizontalDistance(10);
        view.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.CENTERED_NODES);
        view.display(model);
        
        KahinaTreeViewMarker treeMarker = new KahinaTreeViewMarker(model);
        viewPanel = new KahinaTreeViewPanel(treeMarker, kahina);
        JScrollPane viewScrollPane = new JScrollPane(viewPanel);
        
        mainPanel.add(viewScrollPane);
        
        add(mainPanel);
        
        viewPanel.setView(view);
        
        growthProcess = null;
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("addChild"))
        {
            Integer parentNode = view.getMarkedNode();
            if (parentNode == null)
            {
                hint("First select the parent node, then click on this button to add a child.", Color.RED);
            }
            else
            {
                String caption = JOptionPane.showInputDialog(this,"Node caption:", "New node",JOptionPane.PLAIN_MESSAGE);
                addNode(parentNode, caption);
                view.resetAllStructures();
                view.calculateCoordinates();
                viewPanel.updateDisplayAndRepaintFromEventDispatchThread();
            }
        }
        else if (s.equals("removeNode"))
        {
            
        }
        else if (s.equals("addRandomNode"))
        {
            int parentNode = (int) (Math.random() * model.getSize());
            String caption = generateRandomNodeCaption();
            view.setMarkedNode(addNode(parentNode, caption));
            view.resetAllStructures();
            view.calculateCoordinates();
            viewPanel.updateDisplay();
        }
        else if (s.equals("randomGrowth"))
        {
            growthMode = true;
            growthProcess = new Thread(new GrowthProcess());
            growthProcess.start();
        }
        else if (s.equals("stopGrowth"))
        {
            growthMode = false;
            view.resetAllStructures();
            view.calculateCoordinates();
            viewPanel.updateDisplay();
        }
        else if (s.equals("discardTree"))
        {
            model.clear();
            int rootID = model.addNode("start", "none", 0);
            model.setRootID(rootID);
            for (TreeAutomaton breakpoint : breakpoints)
            {
                breakpoint.setTree(model);
            }
            view.display(model);
            view.resetAllStructures();
            view.calculateCoordinates();
            viewPanel.updateDisplay();
        }
    }
    
    private int addNode(int parent, String caption)
    {
        System.err.println("Adding node to " + parent + ": " + caption);
        int child = model.addNode(caption, "none", 0);
        model.addChild(parent, child);
        updateTreeAutomata(child);
        return child;
    }
    
    private void updateTreeAutomata(int child)
    {
        for (TreeAutomaton breakpoint : breakpoints)
        {
            breakpoint.process(child);
        }
    }
    
    private String generateRandomNodeCaption()
    {
        StringBuilder caption = new StringBuilder();
        int syllNumber = (int) (Math.random() * 5) + 1;
        for (int i = 0; i < syllNumber; i++)
        {
            caption.append(generateRandomOnset());
            caption.append(generateRandomNucleus());
            caption.append(generateRandomCoda());
        }
        return caption.toString();
    }
    
    private String generateRandomOnset()
    {
        int choice = (int) (Math.random() * 10);
        switch (choice)
        {
            case 0: return "p";
            case 1: return "t";
            case 2: return "k";
            case 3: return "b";
            case 4: return "d";
            case 5: return "g";
            case 6: return "l";
            case 7: return "r";
            case 8: return "m";
            case 9: return "s";
        }
        return "";
    }
    
    private String generateRandomNucleus()
    {
        int choice = (int) (Math.random() * 8);
        switch (choice)
        {
            case 0: return "a";
            case 1: return "i";
            case 2: return "u";
            case 3: return "e";
            case 4: return "o";
            case 5: return "aa";
            case 6: return "ii";
            case 7: return "uu";
        }
        return "";
    }
    
    private String generateRandomCoda()
    {
        int choice = (int) (Math.random() * 3);
        switch (choice)
        {
            case 0: return "";
            case 1: return "n";
            case 2: return "m";
        }
        return "";
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e.getType().equals("treeMatch"))
        {
            processBreakpointEvent((KahinaTreeMatchEvent) e);
        }
    }
    
    private void processBreakpointEvent(KahinaTreeMatchEvent e)
    {
        hint("Breakpoint match: " + e.getBreakpoint().getName(), e.getBreakpoint().getSignalColor());
        view.setNodeBorderColor(e.getNodeID(), e.getBreakpoint().getSignalColor());
        growthMode = false;
        view.resetAllStructures();
        view.calculateCoordinates();
        viewPanel.updateDisplayAndRepaintFromEventDispatchThread();
        view.setMarkedNode(e.getNodeID());       
    }
    
    public void hint(String hint)
    {
        hintPanel.hint(hint);
    }
    
    public void hint(String hint, Color color)
    {
        hintPanel.hint(hint,color);
    }
    
    private class GrowthProcess implements Runnable
    {           
        public void run()
        {
            while (growthMode)
            {
                try
                {
                    Thread.sleep(10);
                }
                catch (InterruptedException e)
                {
                    
                }
                int parentNode = (int) (Math.random() * model.getSize());
                String caption = generateRandomNodeCaption();
                addNode(parentNode, caption);
            }
        }
    }
}
