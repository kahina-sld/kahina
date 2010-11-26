package org.kahina.prolog.profiler;

import org.kahina.core.profiler.DefaultProfileEntryMapper;
import org.kahina.core.profiler.ProfileEntry;

public class PrologProfileEntryMapper extends DefaultProfileEntryMapper
{
	@Override
	public ProfileEntry map(String x)
	{
		return new ProfileEntry(x.substring(0,x.indexOf("(")), "general");
	}
}
