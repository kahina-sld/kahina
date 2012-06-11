package org.kahina.core.visual.breakpoint;

import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaControlPointProfileViewPanel extends KahinaViewPanel<KahinaControlPointProfileView>
{
    JList pointList;
    
    KahinaControlPointViewPanel pointPanel;
    
    public KahinaControlPointProfileViewPanel(KahinaInstance<?, ?, ?> kahina)
    {
        view = null;
        
        KahinaControlPointProfileListener profileListener = new KahinaControlPointProfileListener(this);
        
        this.setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        
        JPanel profileButtonsPanel = new JPanel();
        profileButtonsPanel.setLayout(new BoxLayout(profileButtonsPanel, BoxLayout.PAGE_AXIS));
        
        JButton newProfileButton = new JButton("New Profile");
        newProfileButton.setActionCommand("newProfile");
        newProfileButton.addActionListener(profileListener);
        profileButtonsPanel.add(newProfileButton);
        
        JButton loadProfileButton = new JButton("Load Profile");
        loadProfileButton.setActionCommand("loadProfile");
        loadProfileButton.addActionListener(profileListener);
        profileButtonsPanel.add(loadProfileButton);
        
        JButton saveProfileButton = new JButton("Save Profile");
        saveProfileButton.setActionCommand("saveProfile");
        saveProfileButton.addActionListener(profileListener);
        profileButtonsPanel.add(saveProfileButton);
        
        this.add(profileButtonsPanel);
        
        JPanel profileDisplayPanel = new JPanel();
        profileDisplayPanel.setLayout(new BoxLayout(profileDisplayPanel, BoxLayout.LINE_AXIS));
        
        JPanel profileManagementPanel = new JPanel();
        profileManagementPanel.setLayout(new BoxLayout(profileManagementPanel, BoxLayout.PAGE_AXIS));
        
        JPanel profileManagementButtonPanel = new JPanel();
        profileManagementButtonPanel.setLayout(new BoxLayout(profileManagementButtonPanel, BoxLayout.LINE_AXIS));
        
        JButton newControlPointButton = new JButton("New");
        newControlPointButton.setActionCommand("newControlPoint");
        newControlPointButton.addActionListener(profileListener);
        profileManagementButtonPanel.add(newControlPointButton);
        
        JButton importControlPointButton = new JButton("Import");
        importControlPointButton.setActionCommand("loadControlPoint");
        importControlPointButton.addActionListener(profileListener);
        profileManagementButtonPanel.add(importControlPointButton);
        
        profileManagementPanel.add(profileManagementButtonPanel);
        
        pointList = new JList();
        pointList.setListData(view.getModel().getControlPoints());
        pointList.addListSelectionListener(profileListener);
        JScrollPane listScroller = new JScrollPane(pointList);
        listScroller.setPreferredSize(new Dimension(250, 80));
        listScroller.setMaximumSize(new Dimension(300, 1000));
        listScroller.setAlignmentX(CENTER_ALIGNMENT);
        profileManagementPanel.add(listScroller);
        
        profileDisplayPanel.add(profileManagementPanel);
        
        pointPanel = new KahinaControlPointViewPanel();
        profileDisplayPanel.add(pointPanel);
        
        this.add(profileDisplayPanel);     
    }
    
    public void updateDisplay()
    {
        
    }
}
