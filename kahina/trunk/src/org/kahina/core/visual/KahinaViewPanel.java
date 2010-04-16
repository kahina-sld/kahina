package org.kahina.core.visual;

import java.awt.Graphics;

import javax.swing.JPanel;

import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;

public class KahinaViewPanel<T extends KahinaObject> extends JPanel implements KahinaListener
{
    protected KahinaView<T> v;
    
    public KahinaViewPanel()
    {
        
    }
    
    public void processEvent(KahinaEvent event)
    {
        System.err.println("KahinaViewPanel received event: " + event.toString());
        if (event.getType().equals("redraw"))
        {
            updateDisplay();
        }
    }
    
    public void setView(KahinaView<T> view)
    {
        this.v = view;
        updateDisplay();
        repaint();
    }
    
    public void updateDisplay()
    {
        
    }
    
    public void paintComponent(Graphics canvas)
    {
        String displayString;
        if (v.model != null)
        {
            displayString = v.model.toString();
        }
        else
        {
            displayString = "no info";
        }
        canvas.drawString(displayString, 50, 50);
    }
}
