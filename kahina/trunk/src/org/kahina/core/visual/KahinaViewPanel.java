package org.kahina.core.visual;

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
}
