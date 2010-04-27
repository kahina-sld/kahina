package org.kahina.core.visual;

import javax.swing.JPanel;

import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;

public abstract class KahinaViewPanel<T extends KahinaView<?>> extends JPanel implements KahinaListener
{
	private static final long serialVersionUID = 5677332450070203832L;
	
	public T view;
	
    public void processEvent(KahinaEvent event)
    {
        //System.err.println(this + " received event: " + event.toString());
        if (event.getType().equals("redraw"))
        {
            updateDisplay();
            repaint();
        }
    }
    
    public void setView(T view)
    {
    	this.view = view;
    	updateDisplay();
    	repaint();
    }
    
    public abstract void updateDisplay();
}
