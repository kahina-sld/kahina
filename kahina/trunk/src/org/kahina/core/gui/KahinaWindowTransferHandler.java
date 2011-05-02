package org.kahina.core.gui;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;

import javax.swing.JComponent;
import javax.swing.TransferHandler;

public class KahinaWindowTransferHandler extends TransferHandler
{
	public static DataFlavor dataFlavor;
    
    public KahinaWindowTransferHandler() 
    {
        super();
        dataFlavor = new DataFlavor(KahinaWindow.class, "KahinaWindow");
    }

    protected Transferable createTransferable(JComponent c) 
    {
        if (c instanceof KahinaTransferablePanel) 
        {
            Transferable tra = (KahinaTransferablePanel) c;
            return tra;
        }
        return null;        
    }
    
    public int getSourceActions(JComponent c) 
    {
        if (c instanceof KahinaTransferablePanel) 
        {
            return TransferHandler.MOVE;
        }
        return TransferHandler.NONE;
    }


}
