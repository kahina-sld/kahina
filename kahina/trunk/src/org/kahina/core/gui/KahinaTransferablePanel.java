package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

public class KahinaTransferablePanel extends JPanel implements Transferable 
{
	String title;
	int windowID;
	
	public KahinaTransferablePanel(String title, int windowID)
	{
		this.title = title;
		this.windowID = windowID;
		this.setTransferHandler(new KahinaWindowTransferHandler());
	    this.setLayout(new BorderLayout());
	    this.setBorder(BorderFactory.createTitledBorder(title));
	}
	
	//for now, only transmit the window ID of the dragged pane
	public static DataFlavor getTransferablePanelDataFlavor() 
	{
	    return DataFlavor.stringFlavor;
	}
	
    public void setTitle(String title)
    {
    	this.title = title;
    	((TitledBorder) getBorder()).setTitle(title);
    }

	
	@Override
	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException 
    {
        if(!flavor.equals(DataFlavor.stringFlavor)) throw new UnsupportedFlavorException(flavor);
        return windowID + "";
	}

	@Override
	public DataFlavor[] getTransferDataFlavors() 
	{
		DataFlavor[] flavors = new DataFlavor[1];
		flavors[0] = DataFlavor.stringFlavor;
		return flavors;
	}

	@Override
	public boolean isDataFlavorSupported(DataFlavor flavor) 
	{
		return flavor.equals(DataFlavor.stringFlavor);
	}

}
