package org.kahina.swi.visual.bindings;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaView;
import org.kahina.swi.data.bindings.SWIPrologVariableBindingSet;

public class SWIPrologVariableBindingSetView extends KahinaView<SWIPrologVariableBindingSet>
{

	public SWIPrologVariableBindingSetView(KahinaInstance<?, ?, ?> kahina)
	{
		super(kahina);
	}

	@Override
	public JComponent makePanel()
	{
		SWIPrologVariableBindingSetViewPanel panel = new SWIPrologVariableBindingSetViewPanel();
        kahina.getGuiControl().registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}
