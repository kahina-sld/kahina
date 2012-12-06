package org.kahina.core.visual.chart;

import java.awt.image.BufferedImage;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaRecursiveChartViewPanel extends KahinaChartViewPanel
{
    KahinaRecursiveChartView view;
    
    public KahinaRecursiveChartViewPanel(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        view = null;
    }

    @Override
    public void updateDisplay()
    {
        // TODO Auto-generated method stub
        
    }
}
