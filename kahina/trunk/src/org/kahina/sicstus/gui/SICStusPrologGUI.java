package org.kahina.sicstus.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;
import org.kahina.sicstus.SICStusPrologStep;

public class SICStusPrologGUI extends LogicProgrammingGUI
{

	public SICStusPrologGUI(Class<? extends SICStusPrologStep> stepType, SICStusPrologDebuggerInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
	}

}
