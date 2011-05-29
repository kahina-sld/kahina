package org.kahina.sicstus.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;
import org.kahina.sicstus.SICStusPrologStep;
import org.kahina.tralesld.gui.TraleSLDWindowManager;

public class SICStusPrologGUI extends LogicProgrammingGUI
{
	public SICStusPrologGUI(Class<? extends SICStusPrologStep> stepType, SICStusPrologDebuggerInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
	}
	
	protected KahinaWindowManager createWindowManager(KahinaGUI gui, KahinaController control)
	{
		return new SICStusPrologWindowManager(gui, control);
	}
}
