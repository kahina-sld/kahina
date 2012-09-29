package org.kahina.core.gui.windows;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.IOException;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.event.KahinaWindowEvent;
import org.kahina.core.gui.event.KahinaWindowEventType;

public class KahinaDropTargetListener implements DropTargetListener 
{	 
    KahinaWindow w;
    KahinaInstance<?,?,?,?> kahina;
    
    public KahinaDropTargetListener(KahinaWindow w) 
    {
        this.w = w;
        this.kahina = w.wm.getKahina();
    }

    public void dragEnter(DropTargetDragEvent dtde) 
    {
        System.err.println("Entered drag area!");
    }

    public void dragExit(DropTargetEvent dte) 
    {

    }

    public void dragOver(DropTargetDragEvent dtde) 
    {
        // TODO Auto-generated method stub

    }

    public void drop(DropTargetDropEvent e) 
    {
        try 
        {
            Transferable tr = e.getTransferable();
            if (tr.isDataFlavorSupported(DataFlavor.stringFlavor)) 
            {
                e.acceptDrop(DnDConstants.ACTION_MOVE);

                int winID = Integer.parseInt((String) tr.getTransferData(DataFlavor.stringFlavor));
                if (winID != w.wm.getArrangement().getMainWindowID())
                {
                
	                System.err.println("Moved window " + winID);
	                
	                kahina.dispatchInstanceEvent(new KahinaWindowEvent(KahinaWindowEventType.UNDOCK, winID)); 
	                if (w.isDummyWindow())
	                {
		                if (!w.isTopLevelWindow())
		                {
		                	KahinaWindow embWin = w.getEmbeddingWindow();
		                	embWin.replaceSubwindow(w, w.wm.getWindowByID(winID));
		                	w.wm.getArrangement().setEmbeddingWindowID(winID, embWin.getID());
		                	embWin.validate();
		                	embWin.repaint(); 
		                	kahina.dispatchInstanceEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, winID));
		                }
		                else
		                {
		                    kahina.dispatchInstanceEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, w.getID()));
		                	//move positions of new window to simulate replacement
		                	KahinaWindow newWindow = w.wm.getWindowByID(winID);
		                	newWindow.setLocation(w.getLocation());
		                	newWindow.validate();
		                	newWindow.repaint();
		                }
	                }
	                else
	                {
	                	switch (w.getWindowType())
	                	{
	                		case KahinaWindowType.LIST_WINDOW:
	                		{
	                			KahinaListWindow listWindow = (KahinaListWindow) w;
	                			listWindow.addSubwindow(w.wm.getWindowByID(winID));
	                			break;
	                		}
	                		case KahinaWindowType.TABBED_WINDOW:
	                		{
	                			KahinaTabbedWindow tabbedWindow = (KahinaTabbedWindow) w;
	                			tabbedWindow.addSubwindow(w.wm.getWindowByID(winID));
	                			break;
	                		}
	                		case KahinaWindowType.MAIN_WINDOW:
	                		{
	                			KahinaMainWindow mainWindow = (KahinaMainWindow) w;
	                			mainWindow.addSubwindow(w.wm.getWindowByID(winID));
	                			break;
	                		}
	                	}
	                	kahina.dispatchInstanceEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, winID));
	                }
                }
                else
                {
                	System.err.println("Cannot embed main window!");
                }
	            e.getDropTargetContext().dropComplete(true);          
            } 
            else 
            {
                e.rejectDrop();
            }
        } 
        catch (UnsupportedFlavorException e1) 
        {
            e1.printStackTrace();
        } 
        catch (IOException e1) 
        {
            e1.printStackTrace();
        }
    }

    public void dropActionChanged(DropTargetDragEvent dtde) 
    {
        // TODO Auto-generated method stub

    }

}
