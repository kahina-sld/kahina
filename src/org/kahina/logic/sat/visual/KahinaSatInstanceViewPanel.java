package org.kahina.logic.sat.visual;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.graph.KahinaGraphViewPanel;

public class KahinaSatInstanceViewPanel extends KahinaGraphViewPanel
{
    public KahinaSatInstanceViewPanel(KahinaInstance<?, ?, ?> kahina)
    {       
        super(kahina);
    }
    
    protected void generateMouseListener()
    {
        this.addMouseListener(new KahinaSatInstanceViewListener(this));
    }
}
