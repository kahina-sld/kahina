package org.kahina.core.gui.breakpoint;

import javax.swing.JPanel;

import org.kahina.core.KahinaInstance;
import org.kahina.lp.LogicProgrammingState;

public class ThresholdedBreakpointEditorWindow extends BreakpointEditorWindow
{

	// TODO xml export/import support

	private static final long serialVersionUID = -3420993187702603445L;

	// Can't override fields, so additionally store thresholded edit panel in
	// a variable with a more specific type.
	protected ThresholdedBreakpointEditPanel thresholdedEditPanel;

	private LogicProgrammingState state;

	public ThresholdedBreakpointEditorWindow(KahinaInstance<?, ?, ?> kahina, int breakpointType)
	{
		super(kahina, breakpointType);
	}

	@Override
	protected JPanel buildRightPanel()
	{
		thresholdedEditPanel = new ThresholdedBreakpointEditPanel(control);
		editPanel = thresholdedEditPanel;
		return editPanel;
	}

	@Override
	protected void compileCurrentlyOpenedBreakpoint()
	{
		super.compileCurrentlyOpenedBreakpoint();
		if (curID != -1)
		{
			state.getWarnThresholdByBreakpoint().put(breakpoints.get(curID), thresholdedEditPanel.getThreshold());
		}
	}

	public void loadState(LogicProgrammingState state)
	{
		this.state = state;
		thresholdedEditPanel.setState(state);
	}
}
