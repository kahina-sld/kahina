package org.kahina.core.visual.source;

import java.awt.Component;
import java.io.File;

import javax.swing.BoxLayout;

import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.editor.KahinaMultifileJEditPanel;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaJEditSourceCodeViewPanel extends KahinaViewPanel<KahinaJEditSourceCodeView>
{

	private static final long serialVersionUID = 7794798458761136862L;
	
	private KahinaMultifileJEditPanel editPanel;
	
	public KahinaJEditSourceCodeViewPanel()
	{
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(createEditPanel());
	}

	private Component createEditPanel()
	{
		editPanel = new KahinaMultifileJEditPanel();
		return editPanel;
	}

	@Override
	public void updateDisplay()
	{
		KahinaSourceCodeLocation location = view.getModel();
		if (location != null)
		{
			editPanel.open(new File(location.getAbsolutePath()));
			editPanel.showLine(location.getLineNumber());
		}
	}

}
