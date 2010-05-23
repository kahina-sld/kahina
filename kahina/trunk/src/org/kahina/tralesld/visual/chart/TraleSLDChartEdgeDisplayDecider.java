package org.kahina.tralesld.visual.chart;

import org.kahina.core.visual.chart.KahinaChartEdgeDisplayDecider;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;

public class TraleSLDChartEdgeDisplayDecider extends KahinaChartEdgeDisplayDecider
{
    public TraleSLDChartEdgeDisplayDecider()
    {
        super();
        displayedEdgeStatus.add(TraleSLDChartEdgeStatus.PROSPECTIVE);
        displayedEdgeStatus.add(TraleSLDChartEdgeStatus.SUCCESSFUL);
        //hiddenEdgeLabels.add("subject_head_rule");
        hiddenEdgeLabels.add("head_complement_rule");
    }
}
