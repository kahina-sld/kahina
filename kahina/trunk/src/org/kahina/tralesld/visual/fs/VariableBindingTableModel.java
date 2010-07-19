package org.kahina.tralesld.visual.fs;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.table.AbstractTableModel;

import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class VariableBindingTableModel extends AbstractTableModel
{

	private static final long serialVersionUID = 3379369094266283475L;

	private static final String[] columnNames = new String[] { "Variable", "Type" };

	private String[] variableNames = new String[0];

	private String[] types = new String[0];

	private String[] grisuMessages = new String[0];

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
	public String getValueAt(int rowIndex, int columnIndex)
	{
		if (columnIndex == 0)
		{
			return variableNames[rowIndex];
		} else
		{
			return types[rowIndex];
		}
	}

	public String getGrisuMessage(int rowIndex)
	{
		return grisuMessages[rowIndex];
	}

	public List<Integer> setBindings(TraleSLDVariableBindingSet model, int[] oldSelectedRows)
	{
		Set<String> oldSelectedVariableNames = new TreeSet<String>();
		for (int rowIndex : oldSelectedRows)
		{
			oldSelectedVariableNames.add(variableNames[rowIndex]);
		}
		if (model == null)
		{
			variableNames = new String[0];
			types = new String[0];
			return Collections.emptyList();
		} else
		{
			int size = model.size();
			variableNames = new String[size];
			types = new String[size];
			grisuMessages = new String[size];
			List<Integer> newSelectedRows = new LinkedList<Integer>();
			int i = 0;
			for (TraleSLDVariableBinding binding : model)
			{
				variableNames[i] = binding.varName;
				types[i] = binding.type;
				grisuMessages[i] = binding.fs.toString();
				if (oldSelectedVariableNames.contains(binding.varName))
				{
					newSelectedRows.add(i);
				}
				i++;
			}
			return newSelectedRows;
		}
	}

}
