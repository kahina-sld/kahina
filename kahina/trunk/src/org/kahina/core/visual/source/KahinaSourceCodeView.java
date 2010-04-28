package org.kahina.core.visual.source;

import javax.swing.JComponent;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.visual.KahinaView;

public class KahinaSourceCodeView extends KahinaView<KahinaSourceCodeLocation>
{   
    public JComponent wrapInPanel()
    {
        KahinaSourceCodeViewPanel panel = new KahinaSourceCodeViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
}
