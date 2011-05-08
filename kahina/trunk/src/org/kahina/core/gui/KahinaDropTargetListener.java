package org.kahina.core.gui;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.IOException;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;

public class KahinaDropTargetListener implements DropTargetListener 
{	 
    KahinaWindow w;
    
    public KahinaDropTargetListener(KahinaWindow w) 
    {
        this.w = w;
    }

    public void dragEnter(DropTargetDragEvent dtde) 
    {
        // TODO Auto-generated method stub
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
                
                System.err.println("Moved window " + winID);
                
                KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.UNDOCK, winID)); 
                if (w.isDummyWindow())
                {
	                if (!w.isTopLevelWindow())
	                {
	                	KahinaWindow embWin = w.getEmbeddingWindow();
	                	embWin.replaceSubwindow(w, w.wm.getWindowByID(winID));
	                	embWin.validate();
	                	embWin.repaint(); 
	                    KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, winID));
	                }
	                else
	                {
	                	KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, w.getID()));
	                	//move positions of new window to simulate replacement
	                	KahinaWindow newWindow = w.wm.getWindowByID(winID);
	                	newWindow.setLocation(w.getLocation());
	                	newWindow.validate();
	                	newWindow.repaint();
	                }
                }
                //for now, this only covers the case of a KahinaTabbedWindow
                else
                {
                	KahinaTabbedWindow tabbedWindow = (KahinaTabbedWindow) w;
                	tabbedWindow.addWindow(w.wm.getWindowByID(winID));
                    KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, winID));
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
