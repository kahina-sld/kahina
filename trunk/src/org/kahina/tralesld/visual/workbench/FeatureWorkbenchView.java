package org.kahina.tralesld.visual.workbench;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.data.FeatureWorkbench;

public class FeatureWorkbenchView extends KahinaView<FeatureWorkbench>
{
	public FeatureWorkbenchView(KahinaController control)
	{
		super(control);
	}

	@Override
	public JComponent makePanel(KahinaGUI gui)
	{
		//TODO: make state handling conceptually cleaner
		TraleSLDState state = (TraleSLDState) gui.getKahinaInstance().getState();
		FeatureWorkbenchViewPanel panel = new FeatureWorkbenchViewPanel(state);
        control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

}