package org.kahina.core.visual.breakpoint;

import javax.swing.JComponent;

import org.kahina.core.data.breakpoint.KahinaControlPointProfile;
import org.kahina.core.visual.KahinaView;
import org.kahina.lp.LogicProgrammingInstance;

public class KahinaControlPointProfileView extends KahinaView<KahinaControlPointProfile>
{
    KahinaControlPointView pointView;
    
    public KahinaControlPointProfileView(LogicProgrammingInstance<?,?,?,?> kahina)
    {
        super(kahina);
        //model = new KahinaControlPointProfile(new LogicProgrammingBreakActuator(kahina.getControl()));
        model = new KahinaControlPointProfile(null);
        pointView = new KahinaControlPointView(kahina);
    }

    @Override
    public JComponent makePanel()
    {
        KahinaControlPointProfileViewPanel panel = new KahinaControlPointProfileViewPanel((LogicProgrammingInstance<?,?,?,?>) kahina);
        kahina.getGuiControl().registerListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
    
}
