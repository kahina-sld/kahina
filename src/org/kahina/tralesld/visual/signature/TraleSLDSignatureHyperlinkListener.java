package org.kahina.tralesld.visual.signature;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.tralesld.gui.TraleSLDTypeSelectionEvent;

public class TraleSLDSignatureHyperlinkListener implements HyperlinkListener
{
	private final KahinaInstance<?, ?, ?> kahina;
	
	public TraleSLDSignatureHyperlinkListener(KahinaInstance<?, ?, ?> kahina)
	{
		this.kahina = kahina;
	}
	
	@Override
	public void hyperlinkUpdate(HyperlinkEvent event) 
	{
		if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) 
		{
			if (event.getDescription().startsWith("type:"))
			{
				kahina.dispatchEvent(new TraleSLDTypeSelectionEvent(event.getDescription().substring(5)));
				kahina.dispatchEvent(new KahinaRedrawEvent());
			}
		}
	}	
}
