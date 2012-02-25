package org.kahina.logic.sat.visual;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.graph.KahinaGraphViewPanel;

public class KahinaSatInstanceViewPanel extends KahinaGraphViewPanel
{
    public KahinaSatInstanceViewPanel(KahinaController control)
    {       
        super(control);
    }
    
    protected void generateMouseListener()
    {
        this.addMouseListener(new KahinaSatInstanceViewListener(this));
    }
}
