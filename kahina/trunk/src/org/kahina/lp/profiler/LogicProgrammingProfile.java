package org.kahina.lp.profiler;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.profiler.ProfileEntry;

public class LogicProgrammingProfile
{
	private final  Map<ProfileEntry, Integer> callsByEntry = new HashMap<ProfileEntry, Integer>();
	
	private final  Map<ProfileEntry, Integer> failsByEntry = new HashMap<ProfileEntry, Integer>();
	
	private final  Map<ProfileEntry, Integer> exitsByEntry = new HashMap<ProfileEntry, Integer>();
	
	private final  Map<ProfileEntry, Integer> redosByEntry = new HashMap<ProfileEntry, Integer>();
	
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
}
