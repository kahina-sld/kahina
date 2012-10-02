package org.kahina.prolog.editor;

import java.io.File;

import org.kahina.core.KahinaInstance;
import org.kahina.core.edit.source.KahinaMultifileJEditPanel;
import org.kahina.lp.LogicProgrammingInstance;

public class PrologMultifileJEditPanel extends KahinaMultifileJEditPanel
{
	private static final long serialVersionUID = 2236124422015782405L;
	
	public PrologMultifileJEditPanel(KahinaInstance<?,?,?,?> instance)
    {
       super(instance);
    }

    @Override
	protected PrologJEditPanel createPanel(File file)
	{
		return new PrologJEditPanel(file, instance);
	}

}
