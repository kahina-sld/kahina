package org.kahina.core.gui.windows;

import java.awt.dnd.DropTarget;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.KahinaWindowTransferHandler;
import org.kahina.core.visual.KahinaEmptyView;

public class KahinaDummyWindow extends KahinaDefaultWindow
{

	private static final long serialVersionUID = 6198599746412653039L;

	public KahinaDummyWindow(KahinaWindowManager wm, KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(new KahinaEmptyView(kahina), wm, kahina);
        setSize(300,150);
		mainPanel.setTransferHandler(new KahinaWindowTransferHandler());
        mainPanel.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));
	}
	
	public boolean isDummyWindow()
	{
		return true;
	}
}
