package org.kahina.core.edit.breakpoint;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.breakpoint.KahinaBreakpointProfile;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.chart.KahinaChartViewPanel;

public class KahinaBreakpointProfileEditor extends KahinaView<KahinaBreakpointProfile>
{

    public KahinaBreakpointProfileEditor(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        // TODO Auto-generated constructor stub
    }

    @Override
    public JComponent makePanel()
    {
        KahinaBreakpointProfileEditorPanel panel = new KahinaBreakpointProfileEditorPanel(kahina);
        kahina.getGuiControl().registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }

}
