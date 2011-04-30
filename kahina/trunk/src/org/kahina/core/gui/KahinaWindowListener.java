package org.kahina.core.gui;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import org.kahina.core.visual.tree.KahinaTreeViewContextMenu;

public class KahinaWindowListener extends MouseAdapter
{
	KahinaWindow w;
	
	public KahinaWindowListener(KahinaWindow w)
	{
		this.w = w;
	}
	
	public void setWindow(KahinaWindow w)
	{
		this.w = w;
	}
	
    @Override
	public void mousePressed(MouseEvent e) 
    {
        maybeShowPopup(e);
    }

    @Override
	public void mouseReleased(MouseEvent e) 
    {
        maybeShowPopup(e);
    }

    private void maybeShowPopup(MouseEvent e) 
    {
        if (e.isPopupTrigger()) 
        {
            KahinaWindowContextMenu.getMenu(w).show(e.getComponent(),e.getX(), e.getY());
        }
    }
}
