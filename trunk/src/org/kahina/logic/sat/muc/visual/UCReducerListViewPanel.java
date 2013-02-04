package org.kahina.logic.sat.muc.visual;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneLayout;

import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.io.color.ColorUtil;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.gui.WrapLayout;
import org.kahina.logic.sat.muc.heuristics.ReductionHeuristic;
import org.kahina.logic.sat.muc.task.ReductionAgent;

public class UCReducerListViewPanel extends KahinaViewPanel<UCReducerListView> implements ActionListener
{
    JPanel newReducerPanel;
    private JComboBox heuristicsChooser;
    private JLabel colorLabel;
    private JButton signalColor;
    private JCheckBox modelRotationCheck;
    private JCheckBox autarkyReductionCheck;
    
    JPanel runningReducersPanel;
    
    MUCInstance kahina;
    
    public UCReducerListViewPanel(MUCInstance kahina, Map<String,Class<? extends ReductionHeuristic>> heuristics)
    {
        this.kahina = kahina;
        
        this.setLayout(new BoxLayout(this,BoxLayout.LINE_AXIS));
        
        JPanel leftPanel = new JPanel();
        
        newReducerPanel = new JPanel();
        newReducerPanel.setBorder(BorderFactory.createTitledBorder("Start a new reduction agent at the selected US"));
        
        JLabel heuristicsLabel = new JLabel("Basic heuristics: ");
        newReducerPanel.add(heuristicsLabel);
        
        heuristicsChooser = new JComboBox();
        heuristicsChooser.setActionCommand("chooseHeuristics");
        heuristicsChooser.addActionListener(this);
        heuristicsChooser.setMaximumSize(new Dimension(300,30));
        for (String name : heuristics.keySet())
        {
            heuristicsChooser.addItem(name);
        }
        newReducerPanel.add(heuristicsChooser);
        
        colorLabel = new JLabel("Signal color: ");
        newReducerPanel.add(colorLabel);
        newReducerPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        signalColor = new JButton("Change");
        signalColor.setBackground(Color.RED);
        signalColor.setActionCommand("changeColor");
        signalColor.addActionListener(this);
        newReducerPanel.add(signalColor);
        newReducerPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        modelRotationCheck = new JCheckBox();
        newReducerPanel.add(modelRotationCheck);
        
        JLabel modelRotationLabel = new JLabel("Model rotation: ");
        newReducerPanel.add(modelRotationLabel);
        newReducerPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        autarkyReductionCheck = new JCheckBox();
        newReducerPanel.add(autarkyReductionCheck);
        
        JLabel autarkyReductionLabel = new JLabel("Autarky reduction: ");
        newReducerPanel.add(autarkyReductionLabel);
        newReducerPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        JButton startReducerButton = new JButton("Start");
        startReducerButton.setActionCommand("startReducer");
        startReducerButton.addActionListener(this);
        newReducerPanel.add(startReducerButton);
        
        GroupLayout layout = new GroupLayout(newReducerPanel);
        newReducerPanel.setLayout(layout);
        
        layout.setHorizontalGroup(
            layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                    .addComponent(heuristicsLabel)
                    .addComponent(colorLabel)
                    .addComponent(startReducerButton)
                    .addComponent(modelRotationLabel)
                    .addComponent(autarkyReductionLabel))
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                    .addComponent(heuristicsChooser)
                    .addComponent(signalColor)
                    .addComponent(modelRotationCheck)
                    .addComponent(autarkyReductionCheck)));
        
        layout.setVerticalGroup(
            layout.createSequentialGroup()
               .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                   .addComponent(heuristicsLabel)
                   .addComponent(heuristicsChooser))
               .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                   .addComponent(colorLabel)
                   .addComponent(signalColor))
               .addGroup(layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                   .addComponent(modelRotationLabel)
                   .addComponent(modelRotationCheck))
               .addGroup(layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                   .addComponent(autarkyReductionLabel)
                   .addComponent(autarkyReductionCheck))
               .addComponent(startReducerButton));     
        
        leftPanel.add(newReducerPanel);
        
        /*JButton shortestPathButton = new JButton("Shortest Path");
        shortestPathButton.setActionCommand("shortestPath");
        shortestPathButton.addActionListener(this);
        leftPanel.add(shortestPathButton);*/
        
        this.add(leftPanel);
        
        runningReducersPanel = new JPanel();
        runningReducersPanel.setLayout(new WrapLayout());
        runningReducersPanel.setBorder(BorderFactory.createTitledBorder("Reduction agents"));
        JScrollPane scrollPane = new JScrollPane(runningReducersPanel);
        scrollPane.setLayout(new ScrollPaneLayout());
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        this.add(scrollPane);
    }

    @Override
    public void updateDisplay()
    {
        //System.err.println("UCReducerListViewPanel.updateDisplay()");
        kahina.getLogger().startMeasuring();
        runningReducersPanel.removeAll();
        //System.err.println(view.getModel());
        int size = view.getModel().size();
        if (size == 0)
        {
            runningReducersPanel.add(new JLabel("No candidate choices!"));
        }
        else
        {
            for (ReductionAgent reducer : view.getModel())
            {
                UCReducerPanel panel = new UCReducerPanel(this.view.kahina);
                reducer.setPanel(panel);
                runningReducersPanel.add(panel);
            }
        }
        runningReducersPanel.revalidate();
        kahina.getLogger().endMeasuring("for updating UCReducerListViewPanel");
    }

    @Override
    public void actionPerformed(ActionEvent event)
    {
        String s = event.getActionCommand();
        if (s.equals("changeColor"))
        {
            Color newColor = JColorChooser.showDialog(this,"Choose Background Color",signalColor.getBackground());
            signalColor.setBackground(newColor);
            view.newReducer.setSignalColor(newColor);
        }   
        else if (s.equals("startReducer"))
        {
            System.err.println("UCReducerListViewPanel.startReducer");
            ReductionAgent newReducer = new ReductionAgent(kahina.getState(), kahina.getState().getSelectedStepID(), kahina.getState().getFiles());
            try
            {
                newReducer.setHeuristics(view.heuristics.get(heuristicsChooser.getSelectedItem()).newInstance());
                newReducer.getHeuristics().setSelVarOffset(kahina.getState().getSatInstance().getHighestVar());
                newReducer.setSignalColor(signalColor.getBackground());
                newReducer.setModelRotation(modelRotationCheck.isSelected());
                newReducer.setAutarkyReduction(autarkyReductionCheck.isSelected());               
                signalColor.setBackground(ColorUtil.randomColor());
                view.getModel().add(newReducer);
                newReducer.start();
            }
            catch (IllegalAccessException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (InstantiationException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            updateDisplay();
            kahina.dispatchEvent(new KahinaRedrawEvent());
        }
        else if (s.equals("chooseHeuristics"))
        {
           //at the moment, the changed heuristic does not become relevant before a new reducer is constructed
           //nothing needs to be done in this scenario, because the selected heuristics are stored in the chooser
        }
        else if (s.equals("shortestPath"))
        {
           ColoredPathDAG dag = kahina.getState().getDecisionGraph();
           List<Integer> shortestPath = dag.findShortestPathFromRoot(kahina.getState().getSelectedStepID());
           System.err.println("Shortest path: " + shortestPath);
           
           ReductionAgent newReducer = new ReductionAgent(kahina.getState(), shortestPath.get(0), kahina.getState().getFiles());
           newReducer.setSignalColor(signalColor.getBackground());
           signalColor.setBackground(ColorUtil.randomColor());
           view.getModel().add(newReducer);
           
           updateDisplay();
           
           newReducer.getPath().getPath().clear();
           newReducer.getPath().getPath().addAll(shortestPath);
           newReducer.cancelTasks();
           newReducer.getPanel().displayIdentificationInfo("Shortest path to node " + kahina.getState().getSelectedStepID());
           newReducer.getPanel().displayCurrentStatusInfo(" (of length " + shortestPath.size() + ")");
           newReducer.getPanel().displaySummaryInfo("");
           newReducer.getPanel().requestViewUpdate();
        }
    }
}
