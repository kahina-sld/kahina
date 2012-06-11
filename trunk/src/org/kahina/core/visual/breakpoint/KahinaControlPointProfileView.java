package org.kahina.core.visual.breakpoint;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.breakpoint.KahinaControlPointProfile;
import org.kahina.core.visual.KahinaView;

public class KahinaControlPointProfileView extends KahinaView<KahinaControlPointProfile>
{
    public KahinaControlPointProfileView(KahinaInstance<?, ?, ?> kahina)
    {
        super(kahina);
        model = new KahinaControlPointProfile();
    }

    @Override
    public JComponent makePanel()
    {
        KahinaControlPointProfileViewPanel panel = new KahinaControlPointProfileViewPanel(kahina);
        panel.setView(this);
        return panel;
    }
    
}
