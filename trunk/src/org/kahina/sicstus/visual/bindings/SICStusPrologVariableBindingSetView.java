package org.kahina.sicstus.visual.bindings;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.visual.KahinaView;
import org.kahina.sicstus.data.bindings.SICStusPrologVariableBindingSet;

public class SICStusPrologVariableBindingSetView extends KahinaView<SICStusPrologVariableBindingSet>
{

	public SICStusPrologVariableBindingSetView(KahinaController control)
	{
		super(control);
	}

	@Override
	public JComponent wrapInPanel(KahinaController control)
	{
		SICStusPrologVariableBindingSetViewPanel panel = new SICStusPrologVariableBindingSetViewPanel();
        control.registerListener(KahinaEventTypes.REDRAW, panel);
		panel.setView(this);
		return panel;
	}

}
