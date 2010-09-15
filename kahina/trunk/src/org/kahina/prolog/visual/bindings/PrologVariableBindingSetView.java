package org.kahina.prolog.visual.bindings;

import javax.swing.JComponent;

import org.kahina.core.KahinaRunner;
import org.kahina.core.visual.KahinaView;
import org.kahina.prolog.data.bindings.PrologVariableBindingSet;

public class PrologVariableBindingSetView extends KahinaView<PrologVariableBindingSet>
{

	@Override
	public JComponent wrapInPanel()
	{
		PrologVariableBindingSetViewPanel panel = new PrologVariableBindingSetViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}
