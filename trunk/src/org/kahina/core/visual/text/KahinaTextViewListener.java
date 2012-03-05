package org.kahina.core.visual.text;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JList;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.gui.event.KahinaSelectionEvent;

public class KahinaTextViewListener implements MouseListener
{
    JList list;
    KahinaController control;
    
    public KahinaTextViewListener(KahinaTextViewPanel p)
    {
        list = p.list;
        control = p.view.control;
    }
    
    public void mouseClicked(MouseEvent e) 
    {
        int index = list.locationToIndex(e.getPoint());
        KahinaLineReference ref = (KahinaLineReference) list.getModel().getElementAt(index);
        if (ref.getStepID() != -1)
        {
            control.processEvent(new KahinaSelectionEvent(ref.getStepID()));
        }
    }

    public void mouseEntered(MouseEvent arg0)
    {
        // TODO Auto-generated method stub
        
    }

    public void mouseExited(MouseEvent arg0)
    {
        // TODO Auto-generated method stub
        
    }

    public void mousePressed(MouseEvent arg0)
    {
        // TODO Auto-generated method stub
        
    }

    public void mouseReleased(MouseEvent arg0)
    {
        // TODO Auto-generated method stub
        
    }
}
