package org.kahina.lp.visual.source;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.source.KahinaJEditSourceCodeView;

public class PrologJEditSourceCodeView extends KahinaJEditSourceCodeView
{
	
	public PrologJEditSourceCodeView(KahinaInstance<?, ?, ?> kahina)
	{
		super(kahina);
	}

	@Override
	protected PrologJEditSourceCodeViewPanel createPanel()
	{
		return new PrologJEditSourceCodeViewPanel();
	}

}
