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
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		innerPanel = new JPanel();
		innerPanel.setLayout(new BoxLayout(innerPanel, BoxLayout.Y_AXIS));
		add(new JScrollPane(innerPanel));
	}

	@Override
	public void updateDisplay()
	{
		innerPanel.removeAll();
		QTypeGoal model = view.getModel();
		
		if (model == null)
		{
			return;
		}
		
		TraleSLDFS fs = model.getFS();
		TraleSLDFS tree = model.getTree();
		TraleSLDFS in = model.getIn();
		TraleSLDFS out = model.getOut();
		
		if (fs != null)
		{
			innerPanel.add(util.createFSFrame("fs", fs.toString()));
		}
		
		if (tree != null)
		{
			innerPanel.add(util.createFSFrame("tree", tree.toString()));
		}
		
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
