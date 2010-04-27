package org.kahina.tralesld.visual.fs;

import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDVariableBindingSetView extends KahinaView<TraleSLDVariableBindingSet>
{

	@Override
	public TraleSLDVariableBindingSetViewPanel wrapInPanel()
	{
		TraleSLDVariableBindingSetViewPanel panel = new TraleSLDVariableBindingSetViewPanel();
		panel.setView(this);
		return panel;
	}

}
