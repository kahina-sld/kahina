package org.kahina.core.control;

import org.kahina.core.event.KahinaEvent;

public interface KahinaListener
{
    public void processEvent(KahinaEvent event);
}
