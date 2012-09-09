package org.kahina.prolog.profiler;

import org.kahina.core.profiler.DefaultProfileEntryMapper;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.profiler.LogicProgrammingProfile;
import org.kahina.lp.profiler.LogicProgrammingProfiler;

public class PrologProfiler extends LogicProgrammingProfiler
{
	public PrologProfiler(LogicProgrammingInstance<?,?,?,?> kahina, LogicProgrammingProfile profile)
	{
		super(kahina, new DefaultProfileEntryMapper(), profile);
	}
}
