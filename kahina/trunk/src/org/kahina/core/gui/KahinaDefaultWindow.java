package org.kahina.core.gui;

import javax.swing.JPanel;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;

public class KahinaDefaultWindow extends KahinaWindow
{
    KahinaView v;
    
    public KahinaDefaultWindow(KahinaView v, KahinaWindowManager wm)
    {
    	super(wm);
        setContent(v, wm.control);
        setTitle(v.getTitle());
    }
    
    public void setContent(KahinaView v, KahinaController control)
    {
        this.v = v;
        mainPanel.removeAll();
        mainPanel.add(v.wrapInPanel(control));
    }
    
    public KahinaView getContent()
    {
        return v;
    }
    
    public boolean isContentWindow()
    {
    	return true;
    }
}
