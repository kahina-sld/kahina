package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.Scrollable;
import javax.swing.border.TitledBorder;

public class KahinaTransferablePanel extends JPanel implements Transferable//, Scrollable
{
	private String title;
	int windowID;
    
    int viewportHeight;
    int viewportWidth;
	
	public KahinaTransferablePanel(String title, int windowID)
	{
		this.title = title;
		this.windowID = windowID;
		this.setTransferHandler(new KahinaWindowTransferHandler());
	    this.setLayout(new BorderLayout());
	    //this.setBorder(BorderFactory.createTitledBorder(title));
	}
    
    public void setSize(int width, int height)
    {
        super.setSize(width,height);
        setViewportSize(width, height);
    }
    
    public void setViewportSize(int width, int height)
    {
        this.viewportWidth = width;
        this.viewportHeight = height;
    }
  
	//for now, only transmit the window ID of the dragged pane
	public static DataFlavor getTransferablePanelDataFlavor() 
	{
	    return DataFlavor.stringFlavor;
	}
	
    public void setTitle(String title)
    {
    	this.title = title;
    	if (getBorder() != null && getBorder() instanceof TitledBorder)
    	{
    	    ((TitledBorder) getBorder()).setTitle(title);
    	}
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

	public String getTitle() {
		return title;
	}

    public Dimension getPreferredScrollableViewportSize()
    {
        return new Dimension(viewportWidth, viewportHeight);
    }

    public int getScrollableBlockIncrement(Rectangle arg0, int arg1, int arg2)
    {
        // TODO this would need to depend on the type of embedded view, a major weakness!
        return 100;
    }

    public boolean getScrollableTracksViewportHeight()
    {
        return false;
    }

    public boolean getScrollableTracksViewportWidth()
    {
        return false;
    }

    public int getScrollableUnitIncrement(Rectangle arg0, int arg1, int arg2)
    {
        // TODO this would need to depend on the type of embedded view, a major weakness!
        return 10;
    }

}
