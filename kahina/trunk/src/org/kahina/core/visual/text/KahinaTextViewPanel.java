package org.kahina.core.visual.text;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Rectangle;

import javax.swing.JList;
import javax.swing.JScrollPane;

import org.kahina.core.visual.KahinaViewPanel;

public class KahinaTextViewPanel extends KahinaViewPanel<KahinaTextView>
{
    protected JList list;
    JScrollPane listScrollPane;
    //determines how many lines are automatically displayed before and after the lead selection line
    //also determines the minimum height of the component
    int displayContext = 2;
    
    public KahinaTextViewPanel()
    {
        this.setLayout(new GridLayout());
        view = null;
        list = new JList();
        list.setSelectionBackground(Color.YELLOW);
        list.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 12));
        list.addMouseListener(new KahinaTextViewListener(this));
        list.setFixedCellHeight(16);
        
        listScrollPane = new JScrollPane(list);
        this.add(listScrollPane);          
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
            int startIndex = leadIndex - displayContext;
            int endIndex = leadIndex + displayContext;
            if (startIndex < 0)
            {
                startIndex = 0;
                endIndex -= startIndex;
            }
            if (endIndex >= list.getModel().getSize())
            {
                endIndex = list.getModel().getSize() - 1;
            }
            Rectangle r = list.getCellBounds(startIndex, endIndex);
            if (r != null)
            {
                //TODO: this causes nasty swing bugs, try to find a workaround for this
                //list.scrollRectToVisible(r);
            }
        }
        repaint();      
    }   
}
