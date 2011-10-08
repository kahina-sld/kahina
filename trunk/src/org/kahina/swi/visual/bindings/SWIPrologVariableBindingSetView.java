package org.kahina.swi.visual.bindings;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.swi.data.bindings.SWIPrologVariableBindingSet;

public class SWIPrologVariableBindingSetView extends KahinaView<SWIPrologVariableBindingSet>
{

	public SWIPrologVariableBindingSetView(KahinaController control)
	{
		super(control);
	}

	@Override
	public JComponent makePanel(KahinaGUI gui)
	{
		SWIPrologVariableBindingSetViewPanel panel = new SWIPrologVariableBindingSetViewPanel();
        control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}
