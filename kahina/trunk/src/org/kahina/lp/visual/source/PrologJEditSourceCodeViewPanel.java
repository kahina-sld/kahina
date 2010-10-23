package org.kahina.lp.visual.source;

import org.kahina.core.visual.source.KahinaJEditSourceCodeViewPanel;
import org.kahina.lp.editor.PrologMultifileJEditPanel;

public class PrologJEditSourceCodeViewPanel extends KahinaJEditSourceCodeViewPanel
{
	
	private static final long serialVersionUID = -5265391588326013752L;

	@Override
	public PrologMultifileJEditPanel createEditPanel()
	{
		return new PrologMultifileJEditPanel();
	}

}
