package org.kahina.swi.visual.bindings;

import java.awt.BorderLayout;

import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.swi.data.bindings.SWIPrologVariableBindingSet;

public class SWIPrologVariableBindingSetViewPanel extends KahinaViewPanel<SWIPrologVariableBindingSetView>
{

	private static final long serialVersionUID = 1373740134949605494L;

	private static final String[] EMPTY = {};

	private final SWIPrologVariableBindingTableModel tableModel = new SWIPrologVariableBindingTableModel();

	private final JTable table = new JTable(tableModel);

	public SWIPrologVariableBindingSetViewPanel()
	{
		setLayout(new BorderLayout());
		table.setFillsViewportHeight(true);
		add(new JScrollPane(table), BorderLayout.CENTER);
	}

	@Override
	public void updateDisplay()
	{
		SWIPrologVariableBindingSet bindings = view.getModel();

		if (bindings == null)
		{
			tableModel.setBindings(EMPTY, EMPTY);
		} else
		{
			tableModel.setBindings(bindings.getKeys(), bindings.getValues());
		}
	}

}
