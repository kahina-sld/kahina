package org.kahina.lp.visual.source;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.source.KahinaJEditSourceCodeView;
import org.kahina.lp.LogicProgrammingInstance;

public class PrologJEditSourceCodeView extends KahinaJEditSourceCodeView
{
	public PrologJEditSourceCodeView(LogicProgrammingInstance kahina)
	{
		super(kahina);
	}

	@Override
	protected PrologJEditSourceCodeViewPanel createPanel()
	{
		return new PrologJEditSourceCodeViewPanel(kahina);
	}

}
