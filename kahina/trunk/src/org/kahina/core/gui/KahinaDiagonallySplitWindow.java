package org.kahina.core.gui;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.TitledBorder;

public class KahinaDiagonallySplitWindow extends KahinaWindow
{
    KahinaWindow upperWindow;
    KahinaWindow lowerWindow;
    
    JPanel topPanel;
    JPanel bottomPanel;
    
    public KahinaDiagonallySplitWindow()
    {
        topPanel = new JPanel();
        topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
        topPanel.setBorder(BorderFactory.createTitledBorder("Drag window 1 here!"));
        bottomPanel = new JPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.Y_AXIS));
        bottomPanel.setBorder(BorderFactory.createTitledBorder("Drag window 2 here!"));
        getContentPane().add(new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, bottomPanel));
    }
    
    public void setUpperWindow(KahinaWindow w)
    {
        ((TitledBorder) topPanel.getBorder()).setTitle(w.getTitle());
        topPanel.add(w.getContentPane());
    }
    
    public void setLowerWindow(KahinaWindow w)
    {
        ((TitledBorder) bottomPanel.getBorder()).setTitle(w.getTitle());
        bottomPanel.add(w.getContentPane());
    }
}
