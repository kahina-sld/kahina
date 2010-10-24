package org.kahina.prolog.visual.bindings;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;
import org.kahina.prolog.data.bindings.PrologVariableBindingSet;

public class PrologVariableBindingSetView extends KahinaView<PrologVariableBindingSet>
{

	public PrologVariableBindingSetView(KahinaController control)
	{
		super(control);
	}

	@Override
	public JComponent wrapInPanel(KahinaController control)
	{
		PrologVariableBindingSetViewPanel panel = new PrologVariableBindingSetViewPanel();
        control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}
