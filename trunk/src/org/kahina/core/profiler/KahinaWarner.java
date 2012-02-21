package org.kahina.core.profiler;

import java.util.Map;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaState;
import org.kahina.core.control.KahinaBreakpoint;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaWarnEvent;
import org.kahina.core.control.patterns.KahinaTreeMatchEvent;
import org.kahina.core.util.ObjectUtil;

public class KahinaWarner implements KahinaListener
{
	
	private KahinaInstance<?, ?, ?> kahina;
	
	public KahinaWarner(KahinaInstance<?, ?, ?> kahina)
	{
		this.kahina = kahina;
		KahinaRunner.getControl().registerListener(KahinaEventTypes.TREE_MATCH, this);
	}

	@Override
	public void processEvent(KahinaEvent event)
	{
		if (event instanceof KahinaTreeMatchEvent)
		{
			processTreeMatchEvent((KahinaTreeMatchEvent) event);
		}
	}

	private void processTreeMatchEvent(KahinaTreeMatchEvent event)
	{
		KahinaBreakpoint breakpoint = event.getBreakpoint();
		KahinaState state = kahina.getState();
		Integer threshold = state.getWarnThresholdByBreakpoint().get(breakpoint);
		if (threshold != null)
		{
			Map<KahinaBreakpoint, Integer> matchCountByBreakpoint = state.getMatchCountByBreakpoint();
			int newMatchCount = ObjectUtil.nullToZero(matchCountByBreakpoint.get(breakpoint)) + 1;
			if (newMatchCount == threshold)
			{
				KahinaRunner.processEvent(new KahinaWarnEvent(breakpoint, newMatchCount));
				newMatchCount = 0;
			}
			matchCountByBreakpoint.put(breakpoint, newMatchCount);
		}
	}

}
