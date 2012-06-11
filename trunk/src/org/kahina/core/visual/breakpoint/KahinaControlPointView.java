package org.kahina.core.visual.breakpoint;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.visual.KahinaView;

public class KahinaControlPointView extends KahinaView<KahinaControlPoint>
{
    public KahinaControlPointView(KahinaInstance<?, ?, ?> kahina)
    {
        super(kahina);
        // TODO Auto-generated constructor stub
    }

    @Override
    public JComponent makePanel()
    {
        KahinaControlPointViewPanel panel = new KahinaControlPointViewPanel();
        panel.setView(this);
        return panel;
    }
    
}
