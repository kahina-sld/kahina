package org.kahina.tralesld.visual.signature;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.kahina.core.KahinaRunner;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.tralesld.event.TraleSLDTypeSelectionEvent;

public class TraleSLDSignatureHyperlinkListener implements HyperlinkListener
{
	@Override
	public void hyperlinkUpdate(HyperlinkEvent event) 
	{
		if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) 
		{
			if (event.getDescription().startsWith("type:"))
			{
				KahinaRunner.processEvent(new TraleSLDTypeSelectionEvent(event.getDescription().substring(5)));
				KahinaRunner.processEvent(new KahinaRedrawEvent());
			}
		}
	}	
}
