package org.kahina.core.visual.breakpoint;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaControlPointViewPanel extends KahinaViewPanel<KahinaControlPointView>
{
    JPanel noSelectionPanel;
    
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
            
        }
        else
        {
            this.removeAll();
            this.add(noSelectionPanel);
        }
    }   
}
