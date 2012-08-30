package org.kahina.lp.visual.source;

import org.kahina.core.visual.source.KahinaJEditSourceCodeViewPanel;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.prolog.editor.PrologMultifileJEditPanel;

public class PrologJEditSourceCodeViewPanel extends KahinaJEditSourceCodeViewPanel
{
	public PrologJEditSourceCodeViewPanel(LogicProgrammingInstance instance)
    {
        super(instance);
    }

    private static final long serialVersionUID = -5265391588326013752L;

	@Override
	public PrologMultifileJEditPanel createEditPanel(LogicProgrammingInstance instance)
	{
		return new PrologMultifileJEditPanel(instance);
	}

}
