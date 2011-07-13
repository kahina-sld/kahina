package org.kahina.lp.visual.source;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.source.KahinaJEditSourceCodeView;

public class PrologJEditSourceCodeView extends KahinaJEditSourceCodeView
{
	
	public PrologJEditSourceCodeView(KahinaController control)
	{
		super(control);
	}

	@Override
	protected PrologJEditSourceCodeViewPanel createPanel()
	{
		return new PrologJEditSourceCodeViewPanel();
	}

}
