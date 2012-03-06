package org.kahina.core.gui.windows;

import java.awt.dnd.DropTarget;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.KahinaWindowTransferHandler;
import org.kahina.core.visual.KahinaEmptyView;

public class KahinaDummyWindow extends KahinaDefaultWindow
{
	public KahinaDummyWindow(KahinaWindowManager wm, KahinaController control)
	{
		super(new KahinaEmptyView(wm.getGuiControl()),wm, control);
        setSize(300,150);
		mainPanel.setTransferHandler(new KahinaWindowTransferHandler());
        mainPanel.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));
	}
	
	public boolean isDummyWindow()
	{
		return true;
	}
}
