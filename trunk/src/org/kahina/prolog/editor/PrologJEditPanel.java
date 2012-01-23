package org.kahina.prolog.editor;

import java.io.File;
import java.io.IOException;

import org.gjt.sp.jedit.Mode;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.kahina.core.edit.source.KahinaJEditPanel;
import org.kahina.core.util.FileUtilities;

public class PrologJEditPanel extends KahinaJEditPanel
{
	
	private static final long serialVersionUID = -783118503897150009L;

	public PrologJEditPanel(File file)
	{
		super(file);
	}
	
	@Override
	protected void configureBuffer(JEditBuffer buffer) throws IOException
	{
		Mode mode = new Mode("prolog");
		mode.setProperty("file", FileUtilities.resourceAsTempFile(Mode.class, "/modes/prolog.xml").getAbsolutePath());
		buffer.setMode(mode);
	}

}
