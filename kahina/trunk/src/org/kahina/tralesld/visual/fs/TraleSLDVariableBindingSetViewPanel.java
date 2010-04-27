package org.kahina.tralesld.visual.fs;

import javax.swing.BoxLayout;
import javax.swing.JTabbedPane;

import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDVariableBindingSetViewPanel extends KahinaViewPanel<TraleSLDVariableBindingSetView>
{
	private static final long serialVersionUID = 8545282386910165013L;
	
	private String shownVariable;
	
	private JTabbedPane tabbedPane;
	
	public TraleSLDVariableBindingSetViewPanel()
	{
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		tabbedPane = new JTabbedPane();
		add(tabbedPane);
	}

	@Override
	public void updateDisplay()
	{
		// TODO Auto-generated method stub
	}

}
