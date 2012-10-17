package org.kahina.logic.sat.visual.cnf.graph;

import java.awt.Graphics;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.graph.KahinaGraphViewPanel;

public class KahinaSatInstanceGraphViewPanel extends KahinaGraphViewPanel
{
    public KahinaSatInstanceGraphViewPanel(KahinaInstance<?, ?, ?, ?> kahina)
    {       
        super(kahina);
    }
    
    protected void generateMouseListener()
    {
        this.addMouseListener(new KahinaSatInstanceGraphViewListener(this));
    }
    
    @Override
    public void paintComponent(Graphics cnv)
    {
        if (((KahinaSatInstanceGraphView) view).textDisplay)
        {
            cnv.drawString(((KahinaSatInstanceGraphView) view).displayText, 0, 0);
        }
        else
        {
            try
            {
                Thread.sleep(10);
                super.paintComponent(cnv);
                if (image == null) 
                {
                    return;
                }
                cnv.drawImage(image, 0, 0, this );
            }
            catch (InterruptedException e)
            {
                System.err.println("Sleep interrupted!");
            }    
        }
    }
}
