package org.kahina.tralesld.visual.fs;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDVariableBindingSetView extends KahinaView<TraleSLDVariableBindingSet>
{

	public TraleSLDVariableBindingSetView(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(kahina);
	}

	@Override
	public JComponent makePanel()
	{
		TraleSLDVariableBindingSetViewPanel panel = new TraleSLDVariableBindingSetViewPanel();
		kahina.registerInstanceListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}
