package org.kahina.lp.profiler;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import org.kahina.core.profiler.ProfileEntry;

public class LogicProgrammingProfile implements Serializable
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 4869556554829662187L;
	
	private static final String[] COLUMN_NAMES = {"Category", "Name", "Calls", "Redos", "Exits", "Fails"};

	private final Map<ProfileEntry, Integer> callsByEntry = new HashMap<ProfileEntry, Integer>();
	
	private final Map<ProfileEntry, Integer> failsByEntry = new HashMap<ProfileEntry, Integer>();
	
	private final Map<ProfileEntry, Integer> exitsByEntry = new HashMap<ProfileEntry, Integer>();
	
	private final Map<ProfileEntry, Integer> redosByEntry = new HashMap<ProfileEntry, Integer>();
	
	public void call(ProfileEntry entry)
	{
		count(entry, callsByEntry);
	}
	
	public void redo(ProfileEntry entry)
	{
		count(entry, redosByEntry);
	}

	private void count(ProfileEntry entry, Map<ProfileEntry, Integer> map)
	{
		if (!map.containsKey(entry))
		{
			map.put(entry, 1);
		} else
		{
			map.put(entry, map.get(entry) + 1);
		}
	}

	public void fail(ProfileEntry entry)
	{
		count(entry, failsByEntry);
	}

	public void exit(ProfileEntry entry)
	{
		count(entry, exitsByEntry);
	}
	
	public TableModel getTableModel()
	{
		Set<ProfileEntry> entrySet = new HashSet<ProfileEntry>();
		entrySet.addAll(callsByEntry.keySet());
		entrySet.addAll(redosByEntry.keySet());
		entrySet.addAll(exitsByEntry.keySet());
		entrySet.addAll(failsByEntry.keySet());
		final int size = entrySet.size();
		final String[] category = new String[size];
		final String[] name = new String[size];
		int[] calls = new int[size];
		int[] redos = new int[size];
		int[] exits = new int[size];
		int[] fails = new int[size];
		int i = 0;
		for (ProfileEntry entry : entrySet)
		{
			category[i] = entry.getCategory();
			name[i] = entry.getName();
			calls[i] = nullToZero(callsByEntry.get(entry));
			redos[i] = nullToZero(redosByEntry.get(entry));
			exits[i] = nullToZero(exitsByEntry.get(entry));
			fails[i] = nullToZero(failsByEntry.get(entry));
			i++;
		}
		final int[][] numbers = new int[][] {null, null, calls, redos, exits, fails};
		return new AbstractTableModel()
		{

			/**
			 * 
			 */
			private static final long serialVersionUID = -8811999723282268512L;

			@Override
			public int getColumnCount()
			{
				return 6;
			}

			@Override
			public int getRowCount()
			{
				return size;
			}

			@Override
			public Object getValueAt(int rowIndex, int columnIndex)
			{
				if (columnIndex == 0)
				{
					return category[rowIndex];
				}
				if (columnIndex == 1)
				{
					return name[rowIndex];
				}
				return numbers[columnIndex][rowIndex];
			}
			
			@Override
			public String getColumnName(int columnIndex)
			{
				return COLUMN_NAMES[columnIndex];
			}
			
			@Override
			public Class<?> getColumnClass(int columnIndex)
			{
				if (columnIndex > 1)
				{
					return Integer.class;
				}
				return String.class;
			}
			
		};
	}

	private int nullToZero(Integer integer)
	{
		if (integer == null)
		{
			return 0;
		}
		return integer;
	}
}
