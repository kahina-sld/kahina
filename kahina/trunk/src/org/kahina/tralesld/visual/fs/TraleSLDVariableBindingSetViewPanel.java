package org.kahina.tralesld.visual.fs;

import javax.swing.BoxLayout;
import javax.swing.JTabbedPane;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class TraleSLDVariableBindingSetViewPanel extends KahinaViewPanel<TraleSLDVariableBindingSetView>
{
	private static final long serialVersionUID = 8545282386910165013L;

	private String shownVariable; // TODO preserve shown variable

	private JTabbedPane tabbedPane;

	private VisualizationUtility util = VisualizationUtility.getDefault();

	public TraleSLDVariableBindingSetViewPanel()
	{
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		tabbedPane = new JTabbedPane();
		add(tabbedPane);
	}

	@Override
	public void updateDisplay()
	{
		removeAll();
		TraleSLDVariableBindingSet model = view.getModel();
		if (model != null)
		{
			for (TraleSLDVariableBinding binding : model)
			{
				tabbedPane.addTab(binding.varName, new FSPane(binding.fs.grisuMessage, util));
			}
		}
	}

}
