package org.kahina.prolog.visual.bindings;

import java.awt.BorderLayout;

import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.prolog.data.bindings.PrologVariableBindingSet;

public class PrologVariableBindingSetViewPanel extends KahinaViewPanel<PrologVariableBindingSetView>
{

	private static final long serialVersionUID = 1373740134949605494L;

	private static final String[] EMPTY = {};

	private final PrologVariableBindingTableModel tableModel = new PrologVariableBindingTableModel();

	private final JTable table = new JTable(tableModel);

	public PrologVariableBindingSetViewPanel()
	{
		setLayout(new BorderLayout());
		table.setFillsViewportHeight(true);
		add(new JScrollPane(table), BorderLayout.CENTER);
	}

	@Override
	public void updateDisplay()
	{
		PrologVariableBindingSet bindings = view.getModel();

		if (bindings == null)
		{
			tableModel.setBindings(EMPTY, EMPTY);
		} else
		{
			tableModel.setBindings(bindings.getKeys(), bindings.getValues());
		}
	}

}
