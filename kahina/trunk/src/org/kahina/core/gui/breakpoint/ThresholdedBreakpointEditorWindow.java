package org.kahina.core.gui.breakpoint;

import javax.swing.JPanel;

import org.kahina.core.control.KahinaController;
import org.kahina.lp.LogicProgrammingState;

public class ThresholdedBreakpointEditorWindow extends BreakpointEditorWindow
{

	private static final long serialVersionUID = -3420993187702603445L;

	// Can't override fields, so additionally store thresholded edit panel in
	// a variable with a more specific type.
	protected ThresholdedBreakpointEditPanel thresholdedEditPanel;

	private LogicProgrammingState state;

	public ThresholdedBreakpointEditorWindow(KahinaController control, int breakpointType)
	{
		super(control, breakpointType);
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
		state.getWarnThresholdByBreakpoint().put(breakpoints.get(curID), thresholdedEditPanel.getThreshold());
	}

	public void loadState(LogicProgrammingState state)
	{
		this.state = state;
		thresholdedEditPanel.setState(state);
	}
}
