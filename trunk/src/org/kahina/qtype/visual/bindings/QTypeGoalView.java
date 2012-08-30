package org.kahina.qtype.visual.bindings;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.visual.KahinaView;
import org.kahina.qtype.QTypeDebuggerInstance;
import org.kahina.qtype.data.bindings.QTypeGoal;

public class QTypeGoalView extends KahinaView<QTypeGoal>
{

	public QTypeGoalView(QTypeDebuggerInstance kahina)
	{
		super(kahina);
	}

	@Override
	public JComponent makePanel()
	{
		QTypeGoalViewPanel panel = new QTypeGoalViewPanel();
        kahina.getGuiControl().registerListener(KahinaEventTypes.REDRAW, panel);
		panel.setView(this);
		return panel;
	}

}
