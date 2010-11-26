package org.kahina.prolog.profiler;

import org.kahina.lp.profiler.LogicProgrammingProfile;
import org.kahina.lp.profiler.LogicProgrammingProfiler;

public class PrologProfiler extends LogicProgrammingProfiler
{
	public PrologProfiler(LogicProgrammingProfile profile)
	{
		super(new PrologProfileEntryMapper(), profile);
	}
}
