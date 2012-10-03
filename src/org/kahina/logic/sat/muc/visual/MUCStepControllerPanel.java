package org.kahina.logic.sat.muc.visual;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneLayout;
import javax.swing.border.Border;

import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.bridge.MUCBridge;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;
import org.kahina.logic.sat.muc.gui.WrapLayout;
import org.kahina.logic.sat.muc.task.UCReductionTask;

public class MUCStepControllerPanel extends KahinaViewPanel<MUCStepController> implements ActionListener, MouseListener
{
    JPanel controlPanel;
    JPanel candidatePanel;
    
    JLabel[] candLabels;
    HashMap<Integer,Integer> icToCandLabel;
    
    boolean displayProcessedCandidates = true;
    
    long lastClick = 0;
    public final static long DBL_CLICK_INTERVAL = 200;
    
    private MUCInstance kahina;
    private ReductionManager reductionManager;
    
    int selectedClause = -1;
    
    public MUCStepControllerPanel(MUCInstance kahina)
    {
        super();
        this.kahina = kahina;
        kahina.registerSessionListener("clauseSelection", this);
        
        reductionManager = new ReductionManager(this);
        
        this.setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS));
        controlPanel = new JPanel();
        controlPanel.setBorder(BorderFactory.createTitledBorder("Options & Convenience Controls"));
        controlPanel.setLayout(new BoxLayout(controlPanel,BoxLayout.LINE_AXIS));
        //controlPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        JButton processAllButton = new JButton("Process all candidates");
        processAllButton.setActionCommand("processAll");
        processAllButton.addActionListener(this);
        controlPanel.add(processAllButton);
        JCheckBox displayAllOption = new JCheckBox("Display processed candidates");
        displayAllOption.setActionCommand("displayAll");
        displayAllOption.setSelected(true);
        displayAllOption.addActionListener(this);
        controlPanel.add(displayAllOption);
        JButton buildSubgraphButton = new JButton("Build entire subgraph");
        buildSubgraphButton.setActionCommand("buildSubgraph");
        buildSubgraphButton.addActionListener(this);
        controlPanel.add(buildSubgraphButton);
        //JButton purgeTreeButton = new JButton("Purge tree of SAT nodes");
        //purgeTreeButton.setActionCommand("purge");
        //purgeTreeButton.addActionListener(this);
        //controlPanel.add(purgeTreeButton);
        this.add(controlPanel);
        candidatePanel = new JPanel();
        candidatePanel.setLayout(new WrapLayout());
        candidatePanel.setBorder(BorderFactory.createTitledBorder("Inspect ICs and select a removal candidate."));
        JScrollPane scrollPane = new JScrollPane(candidatePanel);
        scrollPane.setLayout(new ScrollPaneLayout());
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        this.add(scrollPane);
    }
    
    @Override
    public void updateDisplay()
    {
        candidatePanel.removeAll();
        int size = view.ics.length;
        if (size == 0)
        {
            candidatePanel.add(new JLabel("No candidate choices!"));
        }
        candLabels = new JLabel[size];
        icToCandLabel = new HashMap<Integer,Integer>();
        Border labelBorder = BorderFactory.createLineBorder(Color.BLACK);
        for (int i = 0; i < size; i++)
        {
            JLabel candLabel = new JLabel(view.ics[i] + "");
            candLabels[i] = candLabel;
            icToCandLabel.put(view.ics[i], i);
            candLabel.setOpaque(true);
            candLabel.setBorder(labelBorder);
            setColorAccordingToStatus(candLabel, i);
            if (view.icStatus[i] == 1)
            {
                candLabel.setEnabled(false);
                candLabel.setBackground(new Color(102, 153, 102));
                if (displayProcessedCandidates) candidatePanel.add(candLabel);
            }
            else if (view.icStatus[i] == 2)
            {
                candLabel.setEnabled(false);
                candLabel.setBackground(new Color(183, 50, 50));
                if (displayProcessedCandidates) candidatePanel.add(candLabel);
            }
            else
            {
                //set marked node to orange!
                if (view.ics[i] == selectedClause)
                {
                    candLabel.setBackground(new Color(255, 163, 0));
                }
                candLabel.addMouseListener(this);
                candidatePanel.add(candLabel);
            }
        }  
    }
    
    public void updateLabelColors(MUCStep step)
    {
        MUCState state = kahina.getState();
        if (state.retrieve(MUCStep.class, state.getSelectedStepID()) == step)
        {
            view.updateICStatus(step);
            for (int i = 0; i < view.ics.length; i++)
            {
                setColorAccordingToStatus(candLabels[i], i);
            }
            revalidate();
        }
    }
    
    private void setColorAccordingToStatus(JLabel label, int i)
    {
        //set marked node to orange!
        if (view.ics[i] == selectedClause)
        {
            label.setBackground(new Color(255, 163, 0));
        }
        else if (view.icStatus[i] == 1)
        {
            label.setBackground(new Color(102, 153, 102));
        }
        else if (view.icStatus[i] == 2)
        {
            label.setBackground(new Color(183, 50, 50));
        }
        else
        {
            label.setBackground(Color.WHITE);
        }
    }

    @Override
    //sends out control events via its buttons
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        if (command.equals("displayAll"))
        {
            displayProcessedCandidates = !displayProcessedCandidates;
            updateDisplay();
            revalidate();
        }
        else if (command.equals("processAll"))
        {
            MUCState state = kahina.getState();
            MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
            int size = view.ics.length;
            for (int i = 0; i < size; i++)
            {
                if (view.icStatus[i] == 0)
                {
                    UCReductionTask redTask = new UCReductionTask(  null, reductionManager, state.getStatistics(), 
                            ucStep, state.getSelectedStepID(), 
                            view.ics[i], state.getFiles()
                          );
                    reductionManager.addTask(redTask); 
                }
            }
            /*kahina.dispatchEvent(new KahinaControlEvent("begin"));
            //send commands for processing everything
            int size = view.ics.length;
            for (int i = 0; i < size; i++)
            {
                if (view.icStatus[i] == 0)
                {
                    kahina.dispatchEvent(new KahinaControlEvent(view.ics[i] + ""));
                }
            }
           kahina.dispatchEvent(new KahinaControlEvent("end"));*/
        }
        else if (command.equals("buildSubgraph"))
        {
            MUCState state = kahina.getState();
            Set<Integer> alreadyProcessed = new HashSet<Integer>();
            List<Integer> agenda = new LinkedList<Integer>();
            agenda.add(state.getSelectedStepID());
            while (agenda.size() > 0)
            {
                int nextID = agenda.remove(0);
                if (!alreadyProcessed.contains(nextID))
                {
                    MUCStep ucStep = state.retrieve(MUCStep.class, nextID);
                    //send commands for processing everything
                    int size = view.ics.length;
                    for (int i = 0; i < size; i++)
                    {
                        if (view.icStatus[i] == 0)
                        {
                            UCReductionTask redTask = new UCReductionTask(  null, reductionManager, state.getStatistics(), 
                                    ucStep, nextID, 
                                    view.ics[i], state.getFiles()
                                  );
                            reductionManager.addTask(redTask); 
                        }
                    }
                    //TODO: must wait until all previous reduction tasks have finished
                    agenda.addAll(state.getDecisionGraph().getVisibleChildren(nextID));
                    alreadyProcessed.add(nextID);
                }
            }     
            //(new Thread(new GraphFiller())).start();     
        }
        else if (command.equals("purge"))
        {
            kahina.dispatchEvent(new KahinaControlEvent("purge"));
        }
        else
        {
            kahina.dispatchEvent(new KahinaControlEvent(command));
        }
    }

    @Override
    public void mouseClicked(MouseEvent e)
    {
        if (e.getSource() instanceof JLabel)
        {
            int label = Integer.parseInt(((JLabel) e.getSource()).getText());
            long time = System.currentTimeMillis();
            //System.err.println("label: " + label + " interval: " + (time - lastClick) + " = " + time + " - " + lastClick);
            if (time - lastClick > DBL_CLICK_INTERVAL)
            {
                lastClick = time;
                kahina.dispatchEvent(new ClauseSelectionEvent(label));
            }
            else
            {
                MUCState state = kahina.getState();
                MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
                UCReductionTask redTask = new UCReductionTask(  null, reductionManager, state.getStatistics(), 
                                                                ucStep, state.getSelectedStepID(), 
                                                                label, state.getFiles()
                                                              );
                reductionManager.addTask(redTask);
                //kahina.dispatchEvent(new KahinaControlEvent(label + ""));
                lastClick = 0;
            }
        }
    }

    @Override
    public void mouseEntered(MouseEvent e)
    {  
    }

    @Override
    public void mouseExited(MouseEvent e)
    {     
    }

    @Override
    public void mousePressed(MouseEvent e)
    { 
    }

    @Override
    public void mouseReleased(MouseEvent e)
    {    
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof ClauseSelectionEvent)
        {
            processEvent((ClauseSelectionEvent) e);
        }
        else
        {
            super.processEvent(e);
        }
    }
    
    public void processEvent(ClauseSelectionEvent e)
    {
        int oldSelected = selectedClause;
        selectedClause = e.getClauseID();
        if (oldSelected != -1)
        {
            Integer labelID = icToCandLabel.get(oldSelected);
            if (labelID != null)
            {
                setColorAccordingToStatus(candLabels[labelID], labelID);
            }
        }
        if (selectedClause != -1)
        {
            int labelID = icToCandLabel.get(selectedClause);
            candLabels[labelID].setBackground(new Color(255, 163, 0));
        }
        revalidate();
    }
    
    private class ThreadGraphFiller implements Runnable
    {
        @Override
        public void run()
        {
            Set<Integer> alreadyProcessed = new HashSet<Integer>();
            List<Integer> agenda = new LinkedList<Integer>();
            MUCBridge bridge = (MUCBridge) kahina.getBridge();
            agenda.add(bridge.selectedID);
            MUCState state = (MUCState) kahina.getState();
            while (agenda.size() > 0)
            {
                int nextID = agenda.remove(0);
                if (!alreadyProcessed.contains(nextID))
                {
                    kahina.dispatchEvent(new KahinaSelectionEvent(nextID));
                    kahina.dispatchEvent(new KahinaControlEvent("begin"));
                    //send commands for processing everything
                    int size = view.ics.length;
                    for (int i = 0; i < size; i++)
                    {
                        if (view.icStatus[i] == 0)
                        {
                            kahina.dispatchEvent(new KahinaControlEvent(view.ics[i] + ""));
                        }
                    }
                    kahina.dispatchEvent(new KahinaControlEvent("end"));
                    //must wait until bridge is done
                    while (bridge.isInBatchMode())
                    {
                        try
                        {
                            Thread.sleep(200);
                        }
                        catch (InterruptedException e1)
                        {
                            System.err.println("WARNING: waiting was interrupted while bridge in batch mode!");
                            e1.printStackTrace();
                        }
                    }
                    System.err.println("Processing next: " + state.getDecisionGraph().getVisibleChildren(nextID));
                    agenda.addAll(state.getDecisionGraph().getVisibleChildren(nextID));
                    alreadyProcessed.add(nextID);
                }
            }     
        }     
    }
    
    private class ReductionManager extends KahinaTaskManager
    {
        MUCStepControllerPanel panel;
        
        public ReductionManager(MUCStepControllerPanel panel)
        {
            super();
            this.panel = panel;
        }
        
        public void taskFinished(KahinaTask task)
        {
            super.taskFinished(task);
            if (task instanceof UCReductionTask)
            {
                MUCState state = panel.kahina.getState();
                UCReductionTask ucTask = (UCReductionTask) task;
                MUCStep result = ucTask.getResult();
                //attempt was unsuccessful
                if (ucTask.uc == result)
                {
                    //uc and ucID just stay the same
                    state.addAndDistributeUnreducibilityInfo(ucTask.ucID, ucTask.candidate);
                }
                //attempt was successful, we might have arrived at a new UC
                else
                {
                    int stepID = state.registerMUC(result, ucTask.ucID, ucTask.candidate);
                    ucTask.uc.setRemovalLink(ucTask.candidate, stepID);
                }
                panel.updateLabelColors(ucTask.uc);
                kahina.dispatchEvent(new KahinaUpdateEvent(kahina.getState().getSelectedStepID()));
                kahina.dispatchEvent(new KahinaRedrawEvent());
            }
        }
    }
}
