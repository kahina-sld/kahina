package org.kahina.core.visual;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.KahinaObject;

public class KahinaDefaultView extends KahinaView<KahinaObject>
{
    KahinaObject v;
    
    public JComponent wrapInPanel()
    {
        KahinaDefaultViewPanel panel = new KahinaDefaultViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
}
