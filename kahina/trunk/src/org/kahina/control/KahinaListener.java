package org.kahina.control;

import org.kahina.control.event.KahinaEvent;

public interface KahinaListener
{
    public void processEvent(KahinaEvent event);
}
