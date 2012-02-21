package org.kahina.core.visual.graph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class KahinaGraphViewListener extends MouseAdapter implements ActionListener
{
    KahinaGraphViewPanel view;
    MouseEvent lastMouseEvent;
    
    public KahinaGraphViewListener(KahinaGraphViewPanel view)
    {
        this.view = view;
        this.lastMouseEvent = null;
    }
    
    @Override
    public void actionPerformed(ActionEvent e)
    {
        // TODO Auto-generated method stub
        
    }
}
