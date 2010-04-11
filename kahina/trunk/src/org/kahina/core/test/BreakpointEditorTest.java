package org.kahina.core.test;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.breakpoint.BreakpointEditorWindow;

public class BreakpointEditorTest
{
    public static void main(String[] args)
    {
        BreakpointEditorWindow w = new BreakpointEditorWindow(new KahinaController());
        w.setVisible(true);
    }
}
