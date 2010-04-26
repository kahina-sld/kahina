package org.kahina.core.visual;

import javax.swing.JPanel;

import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;

public abstract class KahinaViewPanel<T extends KahinaView<?>> extends JPanel implements KahinaListener
{    
    public void processEvent(KahinaEvent event)
    {
        //System.err.println(this + " received event: " + event.toString());
        if (event.getType().equals("redraw"))
        {
            updateDisplay();
            repaint();
        }
    }
    
    public abstract void setView(T view);
    
    public void updateDisplay()
    {
        
    }
}
