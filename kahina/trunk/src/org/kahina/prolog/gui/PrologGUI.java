package org.kahina.prolog.gui;

import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.prolog.PrologDebuggerInstance;
import org.kahina.prolog.PrologStep;

public class PrologGUI extends LogicProgrammingGUI
{

	public PrologGUI(Class<? extends PrologStep> stepType, PrologDebuggerInstance instance)
	{
		super(stepType, instance);
	}

}
