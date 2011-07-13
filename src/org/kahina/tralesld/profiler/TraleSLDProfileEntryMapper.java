package org.kahina.tralesld.profiler;

import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;

public class TraleSLDProfileEntryMapper implements Mapper<String, ProfileEntry>
{

	@Override
	public ProfileEntry map(String goalDesc)
	{
		if (goalDesc.startsWith("rule("))
		{
			return new ProfileEntry(goalDesc.substring(5, goalDesc.length() - 1), "rule");
		}
		if (goalDesc.startsWith("goal("))
		{
			return new ProfileEntry(goalDesc.substring(5, goalDesc.length() - 1), "goal");
		}
		int index = goalDesc.indexOf('(');
		if (index != -1)
		{
			goalDesc = goalDesc.substring(0, index);
		}
		return new ProfileEntry(goalDesc, "general");
	}

}
