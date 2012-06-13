package org.kahina.core.visual.breakpoint;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaControlPointViewPanel extends KahinaViewPanel<KahinaControlPointView>
{
    JPanel noSelectionPanel;
    
    JButton activationButton;
    JButton colorButton;
    
    JTextField nameEditLine;
    
    public KahinaControlPointViewPanel(KahinaInstance<?, ?, ?> kahina)
    {
        view = null;
        
        this.setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        this.setBorder(new LineBorder(Color.BLACK, 1));
        
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
    
    @Override
    public void updateDisplay()
    {
        if (view != null && view.getModel() != null)
        {
            this.removeAll();
            
            JPanel optionsPanel = new JPanel();
            optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.PAGE_AXIS));
            
            JPanel exportRemovePanel = new JPanel();
            exportRemovePanel.setLayout(new BoxLayout(exportRemovePanel, BoxLayout.LINE_AXIS));
            
            JButton exportPatternButton = new JButton("Export");
            exportPatternButton.setActionCommand("savePattern");
            //exportPatternButton.addActionListener(patternListener);
            exportPatternButton.setMargin(new Insets(0, 5, 0, 5));
            exportRemovePanel.add(exportPatternButton);
            
            JButton removePatternButton = new JButton("Remove");
            removePatternButton.setActionCommand("removePattern");
            //removePatternButton.addActionListener(profileListener);
            removePatternButton.setMargin(new Insets(0, 5, 0, 5));
            exportRemovePanel.add(removePatternButton);
            
            optionsPanel.add(exportRemovePanel);
            
            JPanel activateColorPanel = new JPanel();
            activateColorPanel.setLayout(new BoxLayout(activateColorPanel, BoxLayout.LINE_AXIS));
            
            activationButton = new JButton();
            if (view.getModel().isActive())
            {
                activationButton.setText("Activate");
            }
            else
            {
                activationButton.setText("Deactivate");
            }
            activationButton.setActionCommand("toogleActivation");
            //activationButton.addActionListener(patternListener);
            activationButton.setMargin(new Insets(0, 5, 0, 5));
            activateColorPanel.add(activationButton);
            
            colorButton = new JButton("Color");
            colorButton.setBackground(view.getModel().getSignalColor());
            colorButton.setActionCommand("changeColor");
            //colorButton.addActionListener(patternListener);
            colorButton.setMargin(new Insets(0, 5, 0, 5));
            activateColorPanel.add(colorButton);
            
            optionsPanel.add(activateColorPanel);
            
            JPanel nameSelectionPanel = new JPanel();
            nameSelectionPanel.setLayout(new BoxLayout(nameSelectionPanel, BoxLayout.PAGE_AXIS));
            nameSelectionPanel.setBorder(new LineBorder(Color.BLACK, 1));
            
            JPanel nameControlPanel = new JPanel();
            nameControlPanel.setLayout(new BoxLayout(nameSelectionPanel, BoxLayout.LINE_AXIS));
            
            JLabel nameLabel = new JLabel("Name: ");
            nameControlPanel.add(nameLabel);
            
            JButton suggestNameButton = new JButton("Suggest");
            suggestNameButton.setMargin(new Insets(0, 5, 0, 5));
            suggestNameButton.setActionCommand("suggestName");
            //suggestNameButton.addActionListener(patternListener);
            nameControlPanel.add(suggestNameButton);
            
            nameSelectionPanel.add(nameControlPanel);
            
            nameEditLine = new JTextField();
            nameEditLine.setText(view.getModel().getName());
            //TODO: listener for key strokes, directly adapting the name
            nameSelectionPanel.add(nameEditLine);
            
            optionsPanel.add(nameSelectionPanel);
            
            this.add(optionsPanel);
            
            StepPatternEditorPanel patternEditor = new StepPatternEditorPanel(); 
            
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
