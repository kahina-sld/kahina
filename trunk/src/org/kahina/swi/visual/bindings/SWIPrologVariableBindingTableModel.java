package org.kahina.swi.visual.bindings;

import javax.swing.table.AbstractTableModel;

public class SWIPrologVariableBindingTableModel extends AbstractTableModel
{

	private static final long serialVersionUID = 2591048016192505705L;

	private static final boolean VERBOSE = false;

	private String[][] data = { {}, {} };

	private String[] columnNames = { "Variable", "Value" };

	public void setBindings(String[] keys, String[] values)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".setBingings(" + keys + ", " + values + ")");
		}
		data = new String[][] { keys, values };
		if (keys.length > 0 && Character.isDigit(keys[0].charAt(0)))
		{
			columnNames[0] = "Argument";
		} else
		{
			columnNames[0] = "Variable";
		}
		fireTableStructureChanged();
	}

	@Override
	public String getColumnName(int columnIndex)
	{
		return columnNames[columnIndex];
	}

	@Override
	public int getColumnCount()
	{
		return 2;
	}

	@Override
	public int getRowCount()
	{
		if (VERBOSE)
		{
			System.err.println(this + ".getRowCount() = " + data[0].length);
		}
		return data[0].length;
	}

	@Override
	public Object getValueAt(int rowIndex, int columnIndex)
	{
		return data[columnIndex][rowIndex];
	}
}
