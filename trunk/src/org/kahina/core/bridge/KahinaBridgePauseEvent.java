package org.kahina.core.bridge;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;

/**
 * Event fired by a bridge to indicate that it has left high-speed mode (leap or
 * autocomplete) and is back in the pause state.
 * @author ke
 *
 */
public class KahinaBridgePauseEvent extends KahinaEvent
{

	public KahinaBridgePauseEvent()
	{
		super(KahinaEventTypes.BRIDGE_PAUSE);
	}

}
