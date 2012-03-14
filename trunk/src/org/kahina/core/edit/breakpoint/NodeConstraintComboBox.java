package org.kahina.core.edit.breakpoint;

import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;

public class NodeConstraintComboBox extends JComboBox
{
    public static boolean verbose = false;
    
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
            if (verbose) System.err.println("NodeConstraintComboBox success: marked index " + selectIndex + " for string \"" + s + "\"");
        }
        else
        {
            if (verbose) System.err.println("NodeConstraintComboBox error: could not find index for string \"" + s + "\"");
        }
    }
}
