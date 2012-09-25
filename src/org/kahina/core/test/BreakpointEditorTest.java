package org.kahina.core.test;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.data.agent.KahinaBreakpointType;
import org.kahina.core.gui.breakpoint.BreakpointEditorWindow;

public class BreakpointEditorTest
{
    public static void main(String[] args)
    {
        BreakpointEditorWindow w = new BreakpointEditorWindow(new KahinaDefaultInstance(), KahinaBreakpointType.PRIMARY_BREAKPOINT);
        w.setVisible(true);
    }
}
