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

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;
import org.kahina.core.visual.KahinaEmptyView;
import org.kahina.core.visual.KahinaView;

public class KahinaDummyWindow extends KahinaDefaultWindow
{
	public KahinaDummyWindow(KahinaWindowManager wm)
	{
		super(new KahinaEmptyView(wm.control),wm);
		mainPanel.setTransferHandler(new KahinaWindowTransferHandler());
        mainPanel.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));
	}
	
	public boolean isDummyWindow()
	{
		return true;
	}
}
