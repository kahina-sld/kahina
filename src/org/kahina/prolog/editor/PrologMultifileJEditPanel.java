package org.kahina.prolog.editor;

import java.io.File;

import org.kahina.core.editor.KahinaMultifileJEditPanel;

public class PrologMultifileJEditPanel extends KahinaMultifileJEditPanel
{

	private static final long serialVersionUID = 2236124422015782405L;
	
	@Override
	protected PrologJEditPanel createPanel(File file)
	{
		return new PrologJEditPanel(file);
	}

}
