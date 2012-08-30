package org.kahina.core.visual.source;

import java.awt.Component;
import java.io.File;

import javax.swing.BoxLayout;

import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.edit.source.KahinaMultifileJEditPanel;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.lp.LogicProgrammingInstance;

public class KahinaJEditSourceCodeViewPanel extends KahinaViewPanel<KahinaJEditSourceCodeView>
{

	private static final long serialVersionUID = 7794798458761136862L;
	
	private KahinaMultifileJEditPanel editPanel;
	
	public KahinaJEditSourceCodeViewPanel(LogicProgrammingInstance instance)
	{
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(createEditPanel2(instance));
	}

	private Component createEditPanel2(LogicProgrammingInstance instance)
	{
		editPanel = createEditPanel(instance);
		return editPanel;
	}
	
	protected KahinaMultifileJEditPanel createEditPanel(LogicProgrammingInstance instance)
	{
		return new KahinaMultifileJEditPanel(instance);
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
