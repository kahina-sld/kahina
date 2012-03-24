package org.kahina.sicstus.visual.bindings;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.visual.KahinaView;
import org.kahina.sicstus.data.bindings.SICStusPrologVariableBindingSet;

public class SICStusPrologVariableBindingSetView extends KahinaView<SICStusPrologVariableBindingSet>
{

	public SICStusPrologVariableBindingSetView(KahinaInstance<?, ?, ?> kahina)
	{
		super(kahina);
	}

	@Override
	public JComponent makePanel()
	{
		SICStusPrologVariableBindingSetViewPanel panel = new SICStusPrologVariableBindingSetViewPanel();
        kahina.getGuiControl().registerListener(KahinaEventTypes.REDRAW, panel);
		panel.setView(this);
		return panel;
	}

}
