package org.kahina.tralesld.visual.fs;

import javax.swing.table.AbstractTableModel;

import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class VariableBindingTableModel extends AbstractTableModel
{

	private static final long serialVersionUID = 3379369094266283475L;

	private static final String[] columnNames = new String[] { "Variable", "Type" };

	private String[] variableNames = new String[0];

	private String[] types = new String[0];

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
		return variableNames.length;
	}

	@Override
	public Object getValueAt(int rowIndex, int columnIndex)
	{
		if (columnIndex == 0)
		{
			return variableNames[rowIndex];
		} else
		{
			return types[rowIndex];
		}
	}

	public void setBindings(TraleSLDVariableBindingSet model)
	{
		if (model == null)
		{
			variableNames = new String[0];
			types = new String[0];
		} else
		{
			int size = model.size();
			variableNames = new String[size];
			types = new String[size];
			int i = 0;
			for (TraleSLDVariableBinding binding : model)
			{
				variableNames[i] = binding.varName;
				types[i++] = binding.type;
			}
		}
	}

}
