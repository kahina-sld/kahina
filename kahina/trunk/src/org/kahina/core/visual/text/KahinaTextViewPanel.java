package org.kahina.core.visual.text;

import java.awt.Color;
import java.awt.Font;
import java.awt.Rectangle;

import javax.swing.JList;
import javax.swing.JScrollPane;

import org.kahina.core.visual.KahinaViewPanel;

public class KahinaTextViewPanel extends KahinaViewPanel<KahinaTextView>
{
    JList list;
    JScrollPane listScrollPane;
    
    public KahinaTextViewPanel()
    {
        view = null;
        list = new JList();
        list.setSelectionBackground(Color.YELLOW);
        list.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 12));
        list.addMouseListener(new KahinaTextViewListener(this));
        
        listScrollPane = new JScrollPane(list);
        this.add(listScrollPane);          
        //add selection listener to list
    }
    
    public void setView(KahinaTextView view)
    {
        this.view = view;
        list.setModel(view.getListModel());
        list.setSelectionModel(view.getSelectionModel());
        revalidate();
        updateDisplay();
    }
    
    @Override
    public void updateDisplay()
    {
        Integer leadIndex = view.getSelectionModel().getLeadSelectionIndex();  
        if (leadIndex != null)
        {
            Rectangle r = list.getCellBounds(leadIndex, leadIndex);
            if (r != null)
            {
                list.scrollRectToVisible(r);
            }
        }
        repaint();      
    }   
}
