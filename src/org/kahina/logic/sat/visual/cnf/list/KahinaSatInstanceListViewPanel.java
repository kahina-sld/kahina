package org.kahina.logic.sat.visual.cnf.list;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.MouseListener;

import javax.swing.JList;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.core.visual.text.KahinaTextViewListener;

public class KahinaSatInstanceListViewPanel extends KahinaViewPanel<KahinaSatInstanceListView>
{
    protected JList list;
    JScrollPane listScrollPane;
    
    private final KahinaInstance<?, ?, ?, ?> kahina;
    
    public KahinaSatInstanceListViewPanel(KahinaInstance<?,?,?,?> kahina)
    {
        this.setLayout(new GridLayout());
        this.kahina = kahina;
        view = null;
        list = new JList();
        list.setSelectionBackground(Color.YELLOW);
        list.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 12));
        list.setFixedCellHeight(16);
        
        listScrollPane = new JScrollPane(list);
        this.add(listScrollPane);          
    }
    
    public void setView(KahinaSatInstanceListView view)
    {
        this.view = view;
        list.setModel(view.getListModel());
        //list.setSelectionModel(view.getSelectionModel());
        /*for (MouseListener mouseListener : list.getMouseListeners())
        {
            list.removeMouseListener(mouseListener);
        }
        list.addMouseListener(new KahinaTextViewListener(this, kahina));*/
        this.updateDisplayAndRepaintFromEventDispatchThread();
    }
    
    @Override
    public void updateDisplay()
    {
        repaint();    
    }
}
