package org.kahina.test;

import org.kahina.control.KahinaController;
import org.kahina.gui.breakpoint.BreakpointEditorWindow;

public class BreakpointEditorTest
{
    public static void main(String[] args)
    {
        BreakpointEditorWindow w = new BreakpointEditorWindow(new KahinaController());
        w.setVisible(true);
    }
}
