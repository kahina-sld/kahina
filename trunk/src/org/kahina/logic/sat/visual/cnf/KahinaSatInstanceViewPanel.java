package org.kahina.logic.sat.visual.cnf;

import java.awt.Graphics;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.graph.KahinaGraphViewPanel;

public class KahinaSatInstanceViewPanel extends KahinaGraphViewPanel
{
    public KahinaSatInstanceViewPanel(KahinaInstance<?, ?, ?, ?> kahina)
    {       
        super(kahina);
    }
    
    protected void generateMouseListener()
    {
        this.addMouseListener(new KahinaSatInstanceViewListener(this));
    }
    
    @Override
    public void paintComponent(Graphics cnv)
    {
        if (((KahinaSatInstanceView) view).textDisplay)
        {
            cnv.drawString(((KahinaSatInstanceView) view).displayText, 0, 0);
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
