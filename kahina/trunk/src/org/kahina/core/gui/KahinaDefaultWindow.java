package org.kahina.core.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;

public class KahinaDefaultWindow extends KahinaWindow
{
    KahinaView v;
    
    public KahinaDefaultWindow(KahinaView v, KahinaController control)
    {
        setContent(v, control);
        setTitle(v.getTitle());
    }
    
    public void setContent(KahinaView v, KahinaController control)
    {
        this.getContentPane().removeAll();
        this.v = v;
        this.add(v.wrapInPanel(control));
    }
    
    public KahinaView getContent()
    {
        return v;
    }
}
