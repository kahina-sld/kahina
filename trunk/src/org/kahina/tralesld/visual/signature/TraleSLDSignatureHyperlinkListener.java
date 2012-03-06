package org.kahina.tralesld.visual.signature;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.tralesld.gui.TraleSLDTypeSelectionEvent;

public class TraleSLDSignatureHyperlinkListener implements HyperlinkListener
{
	KahinaController control;
	
	public TraleSLDSignatureHyperlinkListener(KahinaController control)
	{
		this.control = control;
	}
	
	@Override
	public void hyperlinkUpdate(HyperlinkEvent event) 
	{
		if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) 
		{
			if (event.getDescription().startsWith("type:"))
			{
				control.processEvent(new TraleSLDTypeSelectionEvent(event.getDescription().substring(5)));
				control.processEvent(new KahinaRedrawEvent());
			}
		}
	}	
}
