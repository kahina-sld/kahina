package org.kahina.core.gui.event;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;

public class KahinaRedrawEvent extends KahinaEvent
{
    public KahinaRedrawEvent()
    {
        super(KahinaEventTypes.REDRAW);
    }
}
