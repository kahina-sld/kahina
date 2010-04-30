package org.kahina.core.visual.text;

import java.awt.Color;

import javax.swing.JList;

import org.kahina.core.visual.KahinaViewPanel;

public class KahinaTextViewPanel extends KahinaViewPanel<KahinaTextView>
{
    JList list;
    
    public KahinaTextViewPanel()
    {
        view = null;
        list = new JList();
        list.setSelectionBackground(Color.YELLOW);
        
        this.add(list);          
        //add selection listener to list
    }
    
    public void setView(KahinaTextView view)
    {
        list.setModel(view.getListModel());
        revalidate();
        updateDisplay();
    }
    
    @Override
    public void updateDisplay()
    {
        repaint();      
    }   
}
