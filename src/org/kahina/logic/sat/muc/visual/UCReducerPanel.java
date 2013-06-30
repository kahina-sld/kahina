package org.kahina.logic.sat.muc.visual;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;

import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.task.ReductionAgent;

public class UCReducerPanel extends JPanel implements ActionListener
{
    private JLabel identificationLabel;
    private JLabel currentStatusLabel;
    private JLabel summaryLabel;
    private JButton signalColor;
    private JButton commandButton1;
    private JButton commandButton2;
    
    ReductionAgent reducer;
    
    MUCInstance kahina;
    
    public UCReducerPanel(MUCInstance kahina)
    {
        this.setLayout(new BorderLayout());
        Border labelBorder = BorderFactory.createLineBorder(Color.BLACK);
        this.setBorder(labelBorder);
        
        signalColor = new JButton("Change");
        signalColor.setBackground(Color.RED);
        signalColor.setActionCommand("changeColor");
        signalColor.addActionListener(this);
        this.add(signalColor, BorderLayout.LINE_START);
        this.add(Box.createRigidArea(new Dimension(5,0)));
        
        JPanel statusPanel = new JPanel();
        statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.PAGE_AXIS));   
        identificationLabel = new JLabel("General information about this agent will appear here.");
        statusPanel.add(identificationLabel);
        currentStatusLabel = new JLabel("Waiting for initialization to complete.");
        statusPanel.add(currentStatusLabel);
        summaryLabel = new JLabel("No reduction operation has occurred yet.");
        statusPanel.add(summaryLabel);
        this.add(statusPanel,BorderLayout.CENTER);
        
        JPanel commandButtonPanel = new JPanel();
        commandButtonPanel.setLayout(new BorderLayout());
        
        commandButton1 = new JButton("Pause");
        commandButton1.setActionCommand("pause");
        commandButton1.addActionListener(this);
        commandButtonPanel.add(commandButton1, BorderLayout.PAGE_START);
        
        commandButton2 = new JButton("Stop");
        commandButton2.setActionCommand("stop");
        commandButton2.addActionListener(this);
        commandButtonPanel.add(commandButton2, BorderLayout.PAGE_END);
        
        this.add(commandButtonPanel, BorderLayout.LINE_END);
        
        reducer = null;
        this.kahina = kahina;
    }
    
    public void setReducer(ReductionAgent reducer)
    {
        this.reducer = reducer;
        displayColor(reducer.getSignalColor());     
    }
    
    public String getIdentificationInfo()
    {
        return identificationLabel.getText();
    }
    
    public String getCurrentStatusInfo()
    {
        return currentStatusLabel.getText();
    }
    
    public String getSummaryInfo()
    {
        return summaryLabel.getText();
    }
    
    public void displayIdentificationInfo(String identificationInfo)
    {
        identificationLabel.setText(identificationInfo);
    }
    
    public void displayCurrentStatusInfo(String currentStatusInfo)
    {
        currentStatusLabel.setText(currentStatusInfo);
    }
    
    public void displaySummaryInfo(String summaryInfo)
    {
        summaryLabel.setText(summaryInfo);
    }
    
    public void displayColor(Color color)
    {
        signalColor.setBackground(color);
    }
    
    public void displayRunningState()
    {
        commandButton1.setText("Pause");
        commandButton1.setActionCommand("pause");
        commandButton2.setText("Stop");
        commandButton2.setActionCommand("stop");
    }
    
    public void displayPausedState()
    {
        commandButton1.setText("Continue");
        commandButton1.setActionCommand("continue");
        commandButton2.setText("Stop");
        commandButton2.setActionCommand("stop");
    }
    
    public void displayCompletedState()
    {
        commandButton1.setText("Hide");
        commandButton1.setActionCommand("hide");
        commandButton2.setText("Remove");
        commandButton2.setActionCommand("remove");
    }
    
    public void displayHiddenState()
    {
        commandButton1.setText("Show");
        commandButton1.setActionCommand("show");
        commandButton2.setText("Remove");
        commandButton2.setActionCommand("remove");
    }
    
    //is used by the UCReducer to trigger a refresh of the decision DAG display
    public void requestViewUpdate()
    {
        kahina.dispatchEvent(new KahinaUpdateEvent(kahina.getState().getSelectedStepID()));
        kahina.dispatchEvent(new KahinaRedrawEvent());
    }

    @Override
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("changeColor"))
        {
            Color newColor = JColorChooser.showDialog(this,"Choose Background Color",signalColor.getBackground());
            signalColor.setBackground(newColor);
            reducer.setSignalColor(newColor);
        }
        else if (s.equals("pause"))
        {
            reducer.pauseTasks();
            displayPausedState();
        }
        else if (s.equals("continue"))
        {
            reducer.continueTasks();
        }
        else if (s.equals("stop"))
        {
            reducer.cancelTasks();
        }
        else if (s.equals("show"))
        {
        	reducer.show();
            kahina.getState().getDecisionGraph().addColorPath(reducer.getPath());
        }
        else if (s.equals("hide"))
        {
        	reducer.hide();
            kahina.getState().getDecisionGraph().removeColorPath(reducer.getPath());
        }
        else if (s.equals("remove"))
        {
            kahina.getState().getDecisionGraph().removeColorPath(reducer.getPath());
            kahina.getState().getReducers().remove(reducer);
            this.getParent().remove(this);
        }
        this.validate();
        this.repaint();
        this.requestViewUpdate();
    }
}
