package org.kahina.core.visual.breakpoint;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;

import org.kahina.core.KahinaInstance;
import org.kahina.core.edit.breakpoint.BreakpointEditorEvent;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaControlPointViewPanel extends KahinaViewPanel<KahinaControlPointView>
{
    JPanel noSelectionPanel;
    
    JButton activationButton;
    JButton colorButton;
    
    JTextField nameEditLine;
    
    KahinaControlPointListener pointListener;
    KahinaControlPointProfileListener profileListener;
    
    KahinaInstance<?,?,?> kahina;
    
    //TODO: give some sensible behavior to this constructor
    public KahinaControlPointViewPanel(KahinaInstance<?, ?, ?> kahina)
    {
        this.kahina = kahina;
    }
    
    public KahinaControlPointViewPanel(KahinaInstance<?, ?, ?> kahina, KahinaControlPointProfileListener profileListener)
    {
        this.kahina = kahina;
        this.profileListener = profileListener;
        pointListener = new KahinaControlPointListener(this);
        view = null;
        
        this.setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        this.setBorder(new LineBorder(Color.BLACK, 1));
        
        this.setBackground(Color.CYAN);
        
        noSelectionPanel = new JPanel();
        noSelectionPanel.setLayout(new GridBagLayout());
        JLabel noSelectionLabel = new JLabel("First select or create a new control point.");
        noSelectionPanel.add(noSelectionLabel);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = gbc.weighty = 1.0;
        
        this.add(noSelectionPanel, gbc);     
    }
    
    public void adaptActivationButtonLabel()
    {
        if (view.getModel().isActive())
        {
            activationButton.setText("Deactivate");
            activationButton.setMargin(new Insets(0, 5, 0, 5));
        }
        else
        {
            activationButton.setText("Activate");
            activationButton.setMargin(new Insets(0, 15, 0, 14));
        }
        this.revalidate();
        kahina.dispatchEvent(new BreakpointEditorEvent(BreakpointEditorEvent.BREAKPOINT_NAME_UPDATE));
    }
    
    public void processNameChange()
    {
        nameEditLine.setText(view.getModel().getName());
        this.revalidate();
        kahina.dispatchEvent(new BreakpointEditorEvent(BreakpointEditorEvent.BREAKPOINT_NAME_UPDATE));
    }
    
    @Override
    public void updateDisplay()
    {
        if (view != null && view.getModel() != null)
        {
            this.removeAll();
            
            JPanel optionsPanel = new JPanel();
            optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.PAGE_AXIS));
            optionsPanel.setMinimumSize(new Dimension(200,500));
            optionsPanel.setMaximumSize(new Dimension(200,500));
            
            optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
            
            JPanel exportRemovePanel = new JPanel();
            exportRemovePanel.setLayout(new BoxLayout(exportRemovePanel, BoxLayout.LINE_AXIS));
            
            JButton exportPatternButton = new JButton("Export");
            exportPatternButton.setActionCommand("exportControlPoint");
            exportPatternButton.addActionListener(pointListener);
            exportPatternButton.setMargin(new Insets(0, 5, 0, 5));
            exportRemovePanel.add(exportPatternButton);
            
            JButton removePatternButton = new JButton("Remove");
            removePatternButton.setActionCommand("removeControlPoint");
            removePatternButton.addActionListener(profileListener);
            removePatternButton.setMargin(new Insets(0, 5, 0, 5));
            exportRemovePanel.add(removePatternButton);
            
            optionsPanel.add(exportRemovePanel);
            
            optionsPanel.add(Box.createRigidArea(new Dimension(0,5)));
            
            JPanel activateColorPanel = new JPanel();
            activateColorPanel.setLayout(new BoxLayout(activateColorPanel, BoxLayout.LINE_AXIS));
            
            activationButton = new JButton();
            adaptActivationButtonLabel();
            activationButton.setActionCommand("toggleActivation");
            activationButton.addActionListener(pointListener);
            activateColorPanel.add(activationButton, Component.LEFT_ALIGNMENT);
            
            activateColorPanel.add(Box.createHorizontalGlue());
            
            colorButton = new JButton("Color");
            colorButton.setBackground(view.getModel().getSignalColor());
            colorButton.setActionCommand("changeColor");
            colorButton.addActionListener(pointListener);
            colorButton.setMargin(new Insets(0, 5, 0, 5));
            activateColorPanel.add(colorButton, Component.RIGHT_ALIGNMENT);
            
            optionsPanel.add(activateColorPanel);
            
            optionsPanel.add(Box.createRigidArea(new Dimension(0,5)));
            
            JPanel nameSelectionPanel = new JPanel();
            nameSelectionPanel.setLayout(new BoxLayout(nameSelectionPanel, BoxLayout.PAGE_AXIS));
            nameSelectionPanel.setBorder(new LineBorder(Color.BLACK, 1));
            
            JPanel nameControlPanel = new JPanel();
            nameControlPanel.setLayout(new BoxLayout(nameControlPanel, BoxLayout.LINE_AXIS));
            
            JLabel nameLabel = new JLabel("Name: ");
            nameControlPanel.add(nameLabel, Component.LEFT_ALIGNMENT);
            
            nameControlPanel.add(Box.createHorizontalGlue());
            
            JButton suggestNameButton = new JButton("Suggest");
            suggestNameButton.setMargin(new Insets(0, 5, 0, 5));
            suggestNameButton.setActionCommand("suggestName");
            suggestNameButton.addActionListener(pointListener);
            nameControlPanel.add(suggestNameButton, Component.RIGHT_ALIGNMENT);
            
            nameSelectionPanel.add(nameControlPanel);
            
            nameEditLine = new JTextField();
            nameEditLine.setText(view.getModel().getName());
            nameEditLine.addKeyListener(pointListener);
            nameEditLine.setMinimumSize(new Dimension(180,20));
            nameEditLine.setMaximumSize(new Dimension(180,20));
            //TODO: listener for key strokes, directly adapting the name
            nameSelectionPanel.add(nameEditLine);
            
            optionsPanel.add(nameSelectionPanel);
            
            this.add(optionsPanel);  
            
            this.add(Box.createVerticalGlue());
            
            StepPatternEditorPanel patternEditor = new StepPatternEditorPanel(this); 
            
            //TODO: load pattern into editor; write StepPatternEditorPanel
            
            this.add(patternEditor);
            
        }
        else
        {
            this.removeAll();
            this.add(noSelectionPanel);
        }
    }   
}
