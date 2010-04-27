package org.kahina.tralesld.visual.fs;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDVariableBindingSetViewPanel extends KahinaViewPanel<TraleSLDVariableBindingSetView>
{
	private static final long serialVersionUID = 8545282386910165013L;
	
	private static final boolean verbose = false;

	private String shownVariable; // TODO preserve shown variable

	private JTable table;
	
	private JPanel innerPanel;
	
	VariableBindingTableModel tableModel;

	private VisualizationUtility util = VisualizationUtility.getDefault();

	public TraleSLDVariableBindingSetViewPanel()
	{
		if (verbose)
		{
			System.err.println("TraleSLDVariableBindingSetViewPanel()");
		}
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		tableModel = new VariableBindingTableModel();
		table = new JTable(tableModel);
		JScrollPane tableScrollPane = new JScrollPane(table);
		table.setFillsViewportHeight(true);
		add(tableScrollPane);
		innerPanel = new JPanel();
		add(new JScrollPane(innerPanel));
		if (verbose)
		{
			System.err.println("//TraleSLDVariableBindingSetViewPanel()");
		}
	}

	@Override
	public void updateDisplay()
	{
		if (verbose)
		{
			System.err.println("TraleSLDVariableBindingSetViewPanel.updateDisplay()");
		}
		innerPanel.removeAll();
		tableModel.setBindings(view.getModel());
		if (verbose)
		{
			System.err.println("//TraleSLDVariableBindingSetViewPanel()");
		}
	}

}
