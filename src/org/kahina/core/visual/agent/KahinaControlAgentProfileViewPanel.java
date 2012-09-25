package org.kahina.core.visual.agent;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.edit.breakpoint.BreakpointEditorEvent;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.core.visual.dag.KahinaDAGViewContextMenu;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;

public class KahinaControlAgentProfileViewPanel extends KahinaViewPanel<KahinaControlAgentProfileView>
{
    JList pointList;
    
    KahinaControlAgentViewPanel pointPanel;
    KahinaControlAgentProfileListener profileListener;
    
    KahinaInstance<?,?,?,?> kahina;
    
    public KahinaControlAgentProfileViewPanel(LogicProgrammingInstance<?,?,?,?> kahina)
    {
        view = null;
        
        this.kahina = kahina;
        
        kahina.getControl().registerListener("breakpoint_editor", this);
        
        profileListener = new KahinaControlAgentProfileListener(this, kahina.getState().getStepTree());
        KahinaControlAgentListener pointListener = new KahinaControlAgentListener();
        
        this.setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        
        JPanel profileButtonsPanel = new JPanel();
        profileButtonsPanel.setLayout(new BoxLayout(profileButtonsPanel, BoxLayout.PAGE_AXIS));
        profileButtonsPanel.setBorder(BorderFactory.createTitledBorder("Profile"));
        
        JButton newProfileButton = new JButton("New");
        newProfileButton.setActionCommand("newProfile");
        newProfileButton.addActionListener(profileListener);
        newProfileButton.setMargin(new Insets(0, 5, 0, 5));
        newProfileButton.setMaximumSize(new Dimension(65,25));
        profileButtonsPanel.add(newProfileButton);
        
        JButton loadProfileButton = new JButton("Load");
        loadProfileButton.setActionCommand("loadProfile");
        loadProfileButton.addActionListener(profileListener);
        loadProfileButton.setMargin(new Insets(0, 5, 0, 5));
        loadProfileButton.setMaximumSize(new Dimension(65,25));
        profileButtonsPanel.add(loadProfileButton);
        
        JButton saveProfileButton = new JButton("Save");
        saveProfileButton.setActionCommand("saveProfile");
        saveProfileButton.addActionListener(profileListener);
        saveProfileButton.setMargin(new Insets(0, 5, 0, 5));
        saveProfileButton.setMaximumSize(new Dimension(65,25));
        profileButtonsPanel.add(saveProfileButton);
        
        this.add(profileButtonsPanel);
        
        JPanel profileDisplayPanel = new JPanel();
        profileDisplayPanel.setLayout(new BoxLayout(profileDisplayPanel, BoxLayout.LINE_AXIS));
        profileDisplayPanel.setBorder(BorderFactory.createLineBorder(Color.black));
        
        JPanel profileManagementPanel = new JPanel();
        profileManagementPanel.setLayout(new BoxLayout(profileManagementPanel, BoxLayout.PAGE_AXIS));
        
        JPanel profileManagementButtonPanel = new JPanel();
        profileManagementButtonPanel.setLayout(new BoxLayout(profileManagementButtonPanel, BoxLayout.LINE_AXIS));
        
        JButton newControlPointButton = new JButton("New");
        newControlPointButton.setActionCommand("newControlPoint");
        newControlPointButton.addActionListener(profileListener);
        newControlPointButton.setMargin(new Insets(0, 5, 0, 5));
        profileManagementButtonPanel.add(newControlPointButton);
        
        JButton importControlPointButton = new JButton("Import");
        importControlPointButton.setActionCommand("loadControlPoint");
        importControlPointButton.addActionListener(profileListener);
        importControlPointButton.setMargin(new Insets(0, 5, 0, 5));
        profileManagementButtonPanel.add(importControlPointButton);
        
        profileManagementPanel.add(profileManagementButtonPanel);
        
        pointList = new JList();
        pointList.addListSelectionListener(profileListener);
        JScrollPane listScroller = new JScrollPane(pointList);
        listScroller.setPreferredSize(new Dimension(100, 50));
        listScroller.setMaximumSize(new Dimension(300, 1000));
        listScroller.setAlignmentX(CENTER_ALIGNMENT);
        profileManagementPanel.add(listScroller);
        
        profileDisplayPanel.add(profileManagementPanel);
        
        pointPanel = new KahinaControlAgentViewPanel(kahina, profileListener, pointListener);
        pointList.addMouseListener(new PointListMouseListener(pointPanel, pointListener));
        profileDisplayPanel.add(pointPanel);
        
        this.add(profileDisplayPanel);     
    }
    
    public void setView(KahinaControlAgentProfileView view)
    {
        super.setView(view);
        if (view != null)
        {
            pointPanel.setView(view.pointView);
        }
    }
    
    public void removeCurrentControlAgent()
    {
        view.getModel().removeControlAgent(pointList.getSelectedIndex());
        updateDisplay();
        pointList.setSelectedIndex(-1);
    }
    
    public void updateDisplay()
    {
        if (view.getModel() != null)
        {
            pointList.setListData(view.getModel().getControlPoints());
        }
    }
    
    public void processEvent(KahinaEvent event)
    {
        //System.err.println("processEvent(" + event + ")");
        if (event.getType().equals("breakpoint_editor"))
        {
            processEditorEvent((BreakpointEditorEvent) event);
        }
        else
        {
            super.processEvent(event);
        }
    }
    
    public void processEditorEvent(BreakpointEditorEvent event)
    {
        switch (event.getEditorEventType())
        {
            case BreakpointEditorEvent.BREAKPOINT_NAME_UPDATE:
            {
                pointList.repaint();
                break;
            }
            default:
            {
                //other editor events do not concern the profile view
            }
        }
    }
    
    private class PointListMouseListener extends MouseAdapter
    {
        KahinaControlAgentViewPanel pointView;
        KahinaControlAgentListener pointListener;
        
        public PointListMouseListener(KahinaControlAgentViewPanel pointView, KahinaControlAgentListener pointListener)
        {
            this.pointView = pointView;
            this.pointListener = pointListener;
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
                KahinaControlAgentContextMenu.getMenu(pointListener, profileListener, pointView).show(e.getComponent(),e.getX(), e.getY());
            }
        }
    }

    public KahinaInstance<?, ?, ?, ?> getKahina()
    {
        return kahina;
    }
}
