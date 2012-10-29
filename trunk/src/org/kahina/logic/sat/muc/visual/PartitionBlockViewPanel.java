package org.kahina.logic.sat.muc.visual;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridLayout;

import javax.swing.JList;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.logic.sat.muc.MUCInstance;

public class PartitionBlockViewPanel extends KahinaViewPanel<PartitionBlockView>
{
    private JList list;
    JScrollPane listScrollPane;
    
    public PartitionBlockViewPanel(MUCInstance kahina)
    {
        this.setLayout(new GridLayout());
        view = null;
        list = new JList();
        list.setSelectionBackground(Color.YELLOW);
        list.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 12));
        list.setFixedCellHeight(16);
        list.setCellRenderer(new PartitionBlockListCellRenderer(this));
        list.addMouseListener(new PartitionBlockViewListener(kahina, this));
        
        listScrollPane = new JScrollPane(getList());
        this.add(listScrollPane);  
    }
    
    public void setView(PartitionBlockView view)
    {
        this.view = view;
        getList().setModel(view.getListModel());
        this.updateDisplayAndRepaintFromEventDispatchThread();
    }
    
    @Override
    public void updateDisplay()
    {
        repaint();    
    }

    public JList getList()
    {
        return list;
    }
}
