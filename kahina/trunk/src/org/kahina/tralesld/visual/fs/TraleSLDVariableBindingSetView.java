package org.kahina.tralesld.visual.fs;

import javax.swing.JComponent;

import org.kahina.core.KahinaRunner;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDVariableBindingSetView extends KahinaView<TraleSLDVariableBindingSet>
{

	@Override
	public JComponent wrapInPanel()
	{
		TraleSLDVariableBindingSetViewPanel panel = new TraleSLDVariableBindingSetViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}
