package org.kahina.core.visual;

import java.awt.Graphics;

public class KahinaDefaultViewPanel extends KahinaViewPanel<KahinaDefaultView>
{
    KahinaDefaultView v;
    
    public void setView(KahinaDefaultView view)
    {
        this.v = view;
        updateDisplay();
        repaint();
    }
    
    public void paintComponent(Graphics canvas)
    {
        String displayString;
        if (v.model != null)
        {
            displayString = v.model.toString();
        }
        else
        {
            displayString = "no info";
        }
        canvas.drawString(displayString, 50, 50);
    }
}
