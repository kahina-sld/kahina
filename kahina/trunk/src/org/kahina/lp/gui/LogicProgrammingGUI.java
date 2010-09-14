package org.kahina.lp.gui;

import java.awt.Color;
import java.awt.event.KeyEvent;

import javax.swing.JFrame;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.LogicProgrammingInstance;
import org.kahina.core.breakpoint.KahinaBreakpointType;
import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.breakpoint.BreakpointEditorWindow;
import org.kahina.core.profiler.DefaultProfileEntryMapper;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.gui.profiler.LogicProgrammingProfileWindow;

public class LogicProgrammingGUI extends KahinaGUI
{
	private static final boolean verbose = false;
	
	protected KahinaLayeredTreeView mainTreeView;

	public LogicProgrammingGUI(Class<? extends KahinaStep> stepType, KahinaInstance<?, ?, ?> kahina)
	{
		super(stepType, kahina);
		mainTreeView = generateTreeView();
		mainTreeView.setTitle("Control flow tree");
		KahinaRunner.getControl().registerListener(KahinaEventTypes.UPDATE, mainTreeView);
		views.add(mainTreeView);
		livingViews.add(mainTreeView);
		varNameToView.put("controlFlowTree", mainTreeView);

		getControlPanel().addControlButtonGroup("Control");
		getControlPanel().addControlButton("creep.png", "creep", "(C)ontinue to next step", "Control", KeyEvent.VK_C);
		getControlPanel().addControlButton("roundskip.png", "auto-complete", "(A)uto-complete this step", "Control", KeyEvent.VK_A);
		getControlPanel().addControlButton("pause.png", "(un)pause", "(P)ause the current skip operation", "Control", KeyEvent.VK_P);
		getControlPanel().addControlButton("skip.png", "skip", "(S)kip this step", "Control", KeyEvent.VK_S);
		getControlPanel().addControlButton("reject.png", "fail", "make this step (F)ail", "Control", KeyEvent.VK_F);
		getControlPanel().addControlButton("leap.png", "leap", "(L)eap to next breakpoint match", "Control", KeyEvent.VK_L);
		getControlPanel().addControlButton("stop.png", "stop", "abort skip or leap (X)", "Control", KeyEvent.VK_X);

		getControlPanel().addControlButtonGroup("History");
		getControlPanel().addControlButton("back.png", "backInHistory", "Back (Q)", "History", KeyEvent.VK_Q);
		getControlPanel().addControlButton("forward.png", "forwardInHistory", "Forward (W)", "History", KeyEvent.VK_W);

		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.CALL, Color.WHITE);
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.EXIT, new Color(153, 255, 102));
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.DET_EXIT, new Color(102, 153, 102));
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.FAIL, new Color(183, 50, 50));
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.REDO, new Color(204, 102, 0));
	}
	
	protected KahinaLayeredTreeView generateTreeView()
	{
		return new KahinaLayeredTreeView(true, 0);
	}

	@Override
	protected void displayMainViews()
	{
		mainTreeView.display(((LogicProgrammingState) kahina.getState()).getStepTree());
		mainTreeView.displaySecondaryTree(((LogicProgrammingState) kahina.getState()).getSecondaryStepTree());
	}

	@Override
	protected void processEvent(KahinaDialogEvent e)
	{
		super.processEvent(e);
		switch (e.getDialogEventType())
		{
			case KahinaDialogEvent.PRIMARY_BREAKPOINTS:
			{
				if (verbose)
				{
					System.err.println(this + " received primary breakpoints event.");
				}
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaController(), KahinaBreakpointType.PRIMARY_BREAKPOINT);
				breakpointEditor.setTitle("Edit primary breakpoints");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getPrimaryBreakpoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.SECONDARY_BREAKPOINTS:
			{
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaController(), KahinaBreakpointType.SECONDARY_BREAKPOINT);
				breakpointEditor.setTitle("Edit secondary breakpoints");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getSecondaryBreakpoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.SKIP_POINTS:
			{
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaController(), KahinaBreakpointType.SKIP_POINT);
				breakpointEditor.setTitle("Edit skip points");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getSkipPoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.CREEP_POINTS:
			{
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaController(), KahinaBreakpointType.CREEP_POINT);
				breakpointEditor.setTitle("Edit creep points");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getCreepPoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.FAIL_POINTS:
			{
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaController(), KahinaBreakpointType.FAIL_POINT);
				breakpointEditor.setTitle("Edit fail points");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getFailPoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.FULL_PROFILE:
			{
				JFrame window = new LogicProgrammingProfileWindow(((LogicProgrammingState) kahina.getState()).getFullProfile());
				window.setTitle("Full profile");
				window.setVisible(true);
				break;
			}
			case KahinaDialogEvent.CALL_SUBTREE_PROFILE:
			{
				LogicProgrammingState state = (LogicProgrammingState) kahina.getState();
				JFrame window = new LogicProgrammingProfileWindow(((LogicProgrammingInstance<?, ?, ?>) kahina).getProfiler().profileSubtree(state.getSecondaryStepTree(), state.getSelectedStepID()));
				window.setTitle("Call subtree profile");
				window.setVisible(true);
				break;
			}
			case KahinaDialogEvent.SEARCH_SUBTREE_PROFILE:
			{
				LogicProgrammingState state = (LogicProgrammingState) kahina.getState();
				JFrame window = new LogicProgrammingProfileWindow(((LogicProgrammingInstance<?, ?, ?>) kahina).getProfiler().profileSubtree(state.getStepTree(), state.getSelectedStepID()));
				window.setTitle("Search subtree profile");
				window.setVisible(true);
				break;
			}
		}
	}

	protected Mapper<String, ProfileEntry> getProfileEntryMapper()
	{
		return new DefaultProfileEntryMapper();
	}
}
