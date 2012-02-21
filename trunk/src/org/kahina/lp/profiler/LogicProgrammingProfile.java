package org.kahina.lp.profiler;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.ObjectUtil;

public class LogicProgrammingProfile implements Serializable
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 4869556554829662187L;
	
	private static final String[] COLUMN_NAMES = {"Category", "Name", "Calls", "Redos", "Exits", "Fails", "Exceptions"};

	private static final boolean VERBOSE = false;

	private final Map<ProfileEntry, Integer> callsByEntry = new HashMap<ProfileEntry, Integer>();
	
	private final Map<ProfileEntry, Integer> failsByEntry = new HashMap<ProfileEntry, Integer>();
	
	private final Map<ProfileEntry, Integer> exceptionsByEntry = new HashMap<ProfileEntry, Integer>();
	
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
		if (VERBOSE)
		{
			System.err.println(this + ".fail(" + entry + ")");
		}
		count(entry, failsByEntry);
	}

	public void exception(ProfileEntry entry)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".exception(" + entry + ")");
		}
		count(entry, exceptionsByEntry);
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
		entrySet.addAll(exceptionsByEntry.keySet());
		final int size = entrySet.size() + 1; // additional sum row
		final String[] category = new String[size];
		final String[] name = new String[size];
		int[] calls = new int[size];
		int[] redos = new int[size];
		int[] exits = new int[size];
		int[] fails = new int[size];
		int[] exceptions = new int[size];
		int i = 0;
		int callSum = 0;
		int redoSum = 0;
		int exitSum = 0;
		int failSum = 0;
		int exceptionSum = 0;
		for (ProfileEntry entry : entrySet)
		{
			category[i] = entry.getCategory();
			name[i] = entry.getName();
			callSum += calls[i] = ObjectUtil.nullToZero(callsByEntry.get(entry));
			redoSum += redos[i] = ObjectUtil.nullToZero(redosByEntry.get(entry));
			exitSum += exits[i] = ObjectUtil.nullToZero(exitsByEntry.get(entry));
			failSum += fails[i] = ObjectUtil.nullToZero(failsByEntry.get(entry));
			failSum += fails[i] = ObjectUtil.nullToZero(exceptionsByEntry.get(entry));
			i++;
		}
		category[i] = "Total";
		name[i] = "";
		calls[i] = callSum;
		redos[i] = redoSum;
		exits[i] = exitSum;
		fails[i] = failSum;
		exceptions[i] = exceptionSum;
		final int[][] numbers = new int[][] {null, null, calls, redos, exits, fails, exceptions};
		return new AbstractTableModel()
		{

			private static final long serialVersionUID = -8811999723282268512L;

			@Override
			public int getColumnCount()
			{
				return 7;
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
}
