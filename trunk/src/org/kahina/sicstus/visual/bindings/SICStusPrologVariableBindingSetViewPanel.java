package org.kahina.sicstus.visual.bindings;

import java.awt.BorderLayout;

import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.sicstus.data.bindings.SICStusPrologVariableBindingSet;

public class SICStusPrologVariableBindingSetViewPanel extends KahinaViewPanel<SICStusPrologVariableBindingSetView>
{

	private static final long serialVersionUID = 1373740134949605494L;

	private static final String[] EMPTY = {};

	private final SICStusPrologVariableBindingTableModel tableModel = new SICStusPrologVariableBindingTableModel();

	private final JTable table = new JTable(tableModel);

	public SICStusPrologVariableBindingSetViewPanel()
	{
		setLayout(new BorderLayout());
		table.setFillsViewportHeight(true);
		add(new JScrollPane(table), BorderLayout.CENTER);
	}

	@Override
	public void updateDisplay()
	{
		SICStusPrologVariableBindingSet bindings = view.getModel();

		if (bindings == null)
		{
			tableModel.setBindings(EMPTY, EMPTY, EMPTY);
		} else
		{
			tableModel.setBindings(bindings.getVariableNames(), bindings.getInValues(), bindings.getOutValues());
		}
	}

}
