package org.kahina.core.visual.source;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.visual.KahinaView;

public class KahinaJEditSourceCodeView extends KahinaView<KahinaSourceCodeLocation>
{

	public KahinaJEditSourceCodeView(KahinaInstance<?, ?, ?> kahina)
	{
		super(kahina);
	}

	@Override
	public JComponent makePanel()
	{
		KahinaJEditSourceCodeViewPanel panel = createPanel();
        kahina.getGuiControl().registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	protected KahinaJEditSourceCodeViewPanel createPanel()
	{
		return new KahinaJEditSourceCodeViewPanel();
	}

}
