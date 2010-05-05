package org.kahina.core.visual.text;

import java.awt.Color;
import java.awt.Font;

import javax.swing.JList;
import javax.swing.JScrollPane;

import org.kahina.core.visual.KahinaViewPanel;

public class KahinaTextViewPanel extends KahinaViewPanel<KahinaTextView>
{
    JList list;
    
    public KahinaTextViewPanel()
    {
        view = null;
        list = new JList();
        list.setSelectionBackground(Color.YELLOW);
        list.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 12));
        list.addMouseListener(new KahinaTextViewListener(this));
        
        JScrollPane scrollPane = new JScrollPane(list);
        this.add(scrollPane);          
        //add selection listener to list
    }
    
    public void setView(KahinaTextView view)
    {
        list.setModel(view.getListModel());
        list.setSelectionModel(view.getSelectionModel());
        revalidate();
        updateDisplay();
    }
    
    @Override
    public void updateDisplay()
    {
        repaint();      
    }   
}
