package org.kahina.core.visual.agent;

import javax.swing.JComponent;

import org.kahina.core.data.agent.KahinaControlAgentProfile;
import org.kahina.core.visual.KahinaView;
import org.kahina.lp.LogicProgrammingInstance;

public class KahinaControlAgentProfileView extends KahinaView<KahinaControlAgentProfile>
{
    KahinaControlAgentView pointView;
    
    public KahinaControlAgentProfileView(LogicProgrammingInstance<?,?,?,?> kahina)
    {
        super(kahina);
        display(new KahinaControlAgentProfile(null));
        pointView = new KahinaControlAgentView(kahina);
    }

    @Override
    public JComponent makePanel()
    {
        KahinaControlAgentProfileViewPanel panel = new KahinaControlAgentProfileViewPanel((LogicProgrammingInstance<?,?,?,?>) kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
    
}
