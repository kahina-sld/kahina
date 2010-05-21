package org.kahina.core.gui.breakpoint;

import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;

public class NodeConstraintComboBox extends JComboBox
{
    List<String> options;
    
    public NodeConstraintComboBox(List<String> options)
    {
        super(options.toArray());
        this.options = options;
    }
    
    public void setModel(List<String> options)
    {
        super.setModel(new DefaultComboBoxModel(options.toArray()));
        this.options = options;
    }
    
    public void setSelectedItem(String s)
    {
        int selectIndex = options.indexOf(s);
        if (selectIndex != -1)
        {
            this.setSelectedIndex(selectIndex);
        }
    }
}
