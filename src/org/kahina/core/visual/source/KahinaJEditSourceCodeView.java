package org.kahina.core.visual.source;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.visual.KahinaView;

public class KahinaJEditSourceCodeView extends KahinaView<KahinaSourceCodeLocation>
{

	public KahinaJEditSourceCodeView(KahinaController control)
	{
		super(control);
	}

	@Override
	public JComponent wrapInPanel(KahinaController control)
	{
		KahinaJEditSourceCodeViewPanel panel = createPanel();
        control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	protected KahinaJEditSourceCodeViewPanel createPanel()
	{
		return new KahinaJEditSourceCodeViewPanel();
	}

}
