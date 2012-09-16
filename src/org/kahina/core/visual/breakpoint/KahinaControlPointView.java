package org.kahina.core.visual.breakpoint;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.breakpoint.KahinaControlAgent;
import org.kahina.core.visual.KahinaView;
import org.kahina.lp.data.breakpoint.LogicProgrammingControlAgent;

public class KahinaControlPointView extends KahinaView<KahinaControlAgent>
{
    public KahinaControlPointView(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        // TODO Auto-generated constructor stub
    }

    @Override
    public JComponent makePanel()
    {
        KahinaControlPointViewPanel panel = new KahinaControlPointViewPanel(kahina);
        panel.setView(this);
        return panel;
    }
    
}
