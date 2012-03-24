package org.kahina.core.visual.text;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JList;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.gui.event.KahinaSelectionEvent;

public class KahinaTextViewListener implements MouseListener
{
    JList list;
    private final KahinaInstance<?, ?, ?> kahina;
    
    public KahinaTextViewListener(KahinaTextViewPanel p, KahinaInstance<?, ?, ?> kahina)
    {
        list = p.list;
        this.kahina = kahina;
    }
    
    public void mouseClicked(MouseEvent e) 
    {
        int index = list.locationToIndex(e.getPoint());
        KahinaLineReference ref = (KahinaLineReference) list.getModel().getElementAt(index);
        if (ref.getStepID() != -1)
        {
            kahina.dispatchEvent(new KahinaSelectionEvent(ref.getStepID()));
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
