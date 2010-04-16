package org.kahina.core.visual.source;

import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.chart.KahinaChartViewPanel;

public class KahinaSourceCodeView extends KahinaView<KahinaSourceCodeLocation>
{
    public KahinaSourceCodeViewPanel wrapInPanel()
    {
        KahinaSourceCodeViewPanel panel = new KahinaSourceCodeViewPanel();
        panel.setView(this);
        return panel;
    }
}
