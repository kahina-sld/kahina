package org.kahina.core.visual;

import java.awt.Graphics;

import javax.swing.JPanel;

import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;

public abstract class KahinaViewPanel<T extends KahinaView> extends JPanel implements KahinaListener
{    
    public void processEvent(KahinaEvent event)
    {
        System.err.println("KahinaViewPanel received event: " + event.toString());
        if (event.getType().equals("redraw"))
        {
            updateDisplay();
        }
    }
    
    public abstract void setView(T view);
    
    public void updateDisplay()
    {
        
    }
}
