package org.kahina.parse.visual.chart;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.parse.data.chart.ParseChart;

public class ParseChartView extends KahinaChartView
{
	public ParseChartView(ParseChart chartModel, KahinaInstance<?, ?, ?, ?> kahina) 
	{
		super(chartModel, kahina);
	}
}
