package org.kahina.tralesld.visual.fs;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDVariableBindingSetView extends KahinaView<TraleSLDVariableBindingSet>
{

	public TraleSLDVariableBindingSetView(KahinaController control)
	{
		super(control);
	}

	@Override
	public JComponent makePanel(KahinaGUI gui)
	{
		TraleSLDVariableBindingSetViewPanel panel = new TraleSLDVariableBindingSetViewPanel();
        control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}
