package org.kahina.lp.visual.source;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.source.KahinaJEditSourceCodeView;
import org.kahina.lp.LogicProgrammingInstance;

public class PrologJEditSourceCodeView extends KahinaJEditSourceCodeView
{
    LogicProgrammingInstance<?,?,?,?> kahina;
    
	public PrologJEditSourceCodeView(LogicProgrammingInstance<?,?,?,?> kahina)
	{
		super(kahina);
		this.kahina = kahina;
	}

	@Override
	protected PrologJEditSourceCodeViewPanel createPanel()
	{
		return new PrologJEditSourceCodeViewPanel(kahina);
	}

}
