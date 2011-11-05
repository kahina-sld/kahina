package org.kahina.qtype.visual.bindings;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.qtype.data.bindings.QTypeGoal;

public class QTypeGoalView extends KahinaView<QTypeGoal>
{

	public QTypeGoalView(KahinaController control)
	{
		super(control);
	}

	@Override
	public JComponent makePanel(KahinaGUI gui)
	{
		QTypeGoalViewPanel panel = new QTypeGoalViewPanel();
        control.registerListener(KahinaEventTypes.REDRAW, panel);
		panel.setView(this);
		return panel;
	}

}
