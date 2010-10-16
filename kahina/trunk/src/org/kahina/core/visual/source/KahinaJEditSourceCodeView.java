package org.kahina.core.visual.source;

import javax.swing.JComponent;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.visual.KahinaView;

public class KahinaJEditSourceCodeView extends KahinaView<KahinaSourceCodeLocation>
{

	@Override
	public JComponent wrapInPanel()
	{
		KahinaJEditSourceCodeViewPanel panel = new KahinaJEditSourceCodeViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}
