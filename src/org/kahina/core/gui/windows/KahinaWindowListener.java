package org.kahina.core.gui.windows;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.TransferHandler;

import org.kahina.core.gui.menus.KahinaWindowContextMenu;

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
        if (!e.isPopupTrigger()) 
        {
            TransferHandler handler = w.mainPanel.getTransferHandler();
            handler.exportAsDrag(w.mainPanel, e, TransferHandler.MOVE);
        }
        else
        {
        	 KahinaWindowContextMenu.getMenu(w).show(e.getComponent(),e.getX(), e.getY());
        }
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
