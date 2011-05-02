package org.kahina.core.gui;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

import javax.swing.JPanel;

public class KahinaTransferablePanel extends JPanel implements Transferable 
{
	public KahinaTransferablePanel()
	{
		 this.setTransferHandler(new KahinaWindowTransferHandler());
	}
	
	public static DataFlavor getTransferablePanelDataFlavor() 
	{
	     return KahinaWindowTransferHandler.dataFlavor;
	}

	
	@Override
	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException 
    {
        if(!flavor.equals(KahinaWindowTransferHandler.dataFlavor)) throw new UnsupportedFlavorException(flavor);
        return this;
	}

	@Override
	public DataFlavor[] getTransferDataFlavors() 
	{
		DataFlavor[] flavors = new DataFlavor[1];
		flavors[0] = KahinaWindowTransferHandler.dataFlavor;
		return flavors;
	}

	@Override
	public boolean isDataFlavorSupported(DataFlavor flavor) 
	{
		return flavor.equals(KahinaWindowTransferHandler.dataFlavor);
	}

}
