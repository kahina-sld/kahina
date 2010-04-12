package org.kahina.core.visual;

import java.awt.Graphics;

import javax.swing.JPanel;

import org.kahina.core.data.KahinaObject;

public class KahinaViewPanel<T extends KahinaObject> extends JPanel
{
    protected KahinaView<T> v;
    
    public KahinaViewPanel()
    {
        
    }
    
    public void setView(KahinaView<T> view)
    {
        this.v = view;
        updateDisplay();
        repaint();
    }
    
    public void updateDisplay()
    {
        
    }
    
    public void paint(Graphics canvas)
    {
        String displayString = v.model.toString();
        canvas.drawString(displayString, 50, 50);
    }
}
