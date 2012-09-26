package org.kahina.core.visual.agent;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.visual.KahinaView;
import org.kahina.lp.data.agent.LogicProgrammingControlAgent;

public class KahinaControlAgentView extends KahinaView<KahinaControlAgent>
{
    public KahinaControlAgentView(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        // TODO Auto-generated constructor stub
    }

    @Override
    public JComponent makePanel()
    {
        KahinaControlAgentViewPanel panel = new KahinaControlAgentViewPanel(kahina);
        panel.setView(this);
        return panel;
    }
    
}
