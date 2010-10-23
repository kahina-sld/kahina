package org.kahina.lp.visual.source;

import org.kahina.core.visual.source.KahinaJEditSourceCodeView;

public class PrologJEditSourceCodeView extends KahinaJEditSourceCodeView
{
	
	@Override
	protected PrologJEditSourceCodeViewPanel createPanel()
	{
		return new PrologJEditSourceCodeViewPanel();
	}

}
