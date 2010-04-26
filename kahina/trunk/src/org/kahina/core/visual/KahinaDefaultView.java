package org.kahina.core.visual;

import org.kahina.core.data.KahinaObject;

public class KahinaDefaultView extends KahinaView<KahinaObject>
{
    KahinaObject v;
    
    public KahinaViewPanel<KahinaDefaultView> wrapInPanel()
    {
        KahinaDefaultViewPanel panel = new KahinaDefaultViewPanel();
        panel.setView(this);
        return panel;
    }
}
