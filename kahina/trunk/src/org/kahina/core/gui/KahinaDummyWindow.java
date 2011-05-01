package org.kahina.core.gui;

import org.kahina.core.visual.KahinaEmptyView;
import org.kahina.core.visual.KahinaView;

public class KahinaDummyWindow extends KahinaDefaultWindow
{
	public KahinaDummyWindow(KahinaWindowManager wm)
	{
		super(new KahinaEmptyView(wm.control),wm);
	}
	
	public boolean isDummyWindow()
	{
		return true;
	}
}
