package org.kahina.core.profiler;

import org.kahina.core.util.Mapper;

public class DefaultProfileEntryMapper implements Mapper<String, ProfileEntry>
{
	@Override
	public ProfileEntry map(String x)
	{
		return new ProfileEntry(x, "general");
	}
}
