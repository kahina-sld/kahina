package org.kahina.qtype.visual.bindings;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.visual.fs.VisualizationUtility;

public class QTypeGoalViewPanel extends KahinaViewPanel<QTypeGoalView>
{
	private static final long serialVersionUID = 6523823660347470157L;
	
	private final VisualizationUtility util = new VisualizationUtility();
	
	private final JPanel innerPanel;

	public QTypeGoalViewPanel()
	{
		setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		innerPanel = new JPanel();
		add(new JScrollPane(innerPanel));
	}

	@Override
	public void updateDisplay()
	{
		QTypeGoal model = view.getModel();
		
		if (model == null)
		{
			return;
		}
		
		TraleSLDFS in = model.getIn();
		TraleSLDFS out = model.getOut();
		
		if (in != null)
		{
			innerPanel.add(util.createFSFrame("in", in.toString()));
		}
		
		if (out != null)
		{
			innerPanel.add(util.createFSFrame("out", out.toString()));
		}
	}

}
