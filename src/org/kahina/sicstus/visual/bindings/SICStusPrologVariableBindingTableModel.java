package org.kahina.sicstus.visual.bindings;

import javax.swing.table.AbstractTableModel;

public class SICStusPrologVariableBindingTableModel extends AbstractTableModel
{

	private static final long serialVersionUID = 2591048016192505705L;

	private String[][] data = { {}, {}, {} };

	private String[] columnNames = { "Variable", "Before", "After" };

	public void setBindings(String[] variableNames, String[] inValues, String[] outValues)
	{
		data = new String[][] { variableNames, inValues, outValues };
	}

	@Override
	public String getColumnName(int columnIndex)
	{
		return columnNames[columnIndex];
	}

	@Override
	public int getColumnCount()
	{
		return 3;
	}

	@Override
	public int getRowCount()
	{
		return data[0].length;
	}

	@Override
	public Object getValueAt(int rowIndex, int columnIndex)
	{
		return data[columnIndex][rowIndex];
	}
}
