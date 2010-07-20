package org.kahina.core.gui;

import org.kahina.core.visual.KahinaView;

public class KahinaDefaultWindow extends KahinaWindow
{
    KahinaView v;
    
    public KahinaDefaultWindow(KahinaView v)
    {
        setContent(v);
        setTitle(v.getTitle());
    }
    
    public void setContent(KahinaView v)
    {
        this.getContentPane().removeAll();
        this.v = v;
        this.add(v.wrapInPanel());
    }
    
    public KahinaView getContent()
    {
        return v;
    }
}
