package org.kahina.core.gui;

import java.awt.Point;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.IOException;

import javax.swing.JPanel;

import org.kahina.core.visual.KahinaEmptyView;
import org.kahina.core.visual.KahinaView;

public class KahinaDummyWindow extends KahinaDefaultWindow
{
	public KahinaDummyWindow(KahinaWindowManager wm)
	{
		super(new KahinaEmptyView(wm.control),wm);
		mainPanel.setTransferHandler(new KahinaWindowTransferHandler());
        mainPanel.setDropTarget(new DropTarget(mainPanel, new MyDropTargetListener(mainPanel)));
	}
	
	public boolean isDummyWindow()
	{
		return true;
	}
	
	class MyDropTargetListener implements DropTargetListener 
	{	 
        JPanel dropPanel;
        
        public MyDropTargetListener(JPanel dropPanel) 
        {
            this.dropPanel = dropPanel;
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
                DataFlavor flav = KahinaTransferablePanel.getTransferablePanelDataFlavor();
                if (tr.isDataFlavorSupported(flav)) 
                {
                    System.out.println("Drop accepted as movement!");
                    e.acceptDrop(DnDConstants.ACTION_MOVE);
 
                    KahinaTransferablePanel data = (KahinaTransferablePanel) tr.getTransferData(flav);
                    
                    Point loc = e.getLocation();
 
                    data.setLocation(loc);      
                    dropPanel.add(data);
                    dropPanel.updateUI();
                    e.getDropTargetContext().dropComplete(true);
                } 
                else 
                {
                    System.err.println ("DataFlavor.stringFlavor is not supported, rejected");
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

}
