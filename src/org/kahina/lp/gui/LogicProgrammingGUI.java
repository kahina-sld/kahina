package org.kahina.lp.gui;

import java.awt.Color;
import java.awt.event.KeyEvent;

import javax.swing.JFrame;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.data.breakpoint.KahinaBreakpointType;
import org.kahina.core.edit.breakpoint.KahinaBreakpointProfileEditor;
import org.kahina.core.gui.KahinaDialogEvent;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.breakpoint.BreakpointEditorWindow;
import org.kahina.core.gui.breakpoint.ThresholdedBreakpointEditorWindow;
import org.kahina.core.profiler.DefaultProfileEntryMapper;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;
import org.kahina.core.visual.tree.KahinaAbstractTreeView;
import org.kahina.core.visual.tree.KahinaListTreeView;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.gui.profiler.LogicProgrammingProfileWindow;

public class LogicProgrammingGUI extends KahinaGUI
{
	private static final boolean VERBOSE = false;

	protected KahinaAbstractTreeView mainTreeView;
	//protected KahinaLayeredTreeView mainTreeView;
	
	protected KahinaBreakpointProfileEditor breakpointProfileEditor;

	public LogicProgrammingGUI(Class<? extends KahinaStep> stepType, KahinaInstance<?, ?, ?> kahina)
	{
		super(stepType, kahina);
		
		mainTreeView = generateTreeView();
		mainTreeView.setTitle("Control flow tree");
		kahina.getGuiControl().registerListener(KahinaEventTypes.UPDATE, mainTreeView);
		views.add(mainTreeView);
		livingViews.add(mainTreeView);
		varNameToView.put("controlFlowTree", mainTreeView);
		
		breakpointProfileEditor = new KahinaBreakpointProfileEditor(kahina);
	    breakpointProfileEditor.setTitle("Breakpoint Profile");
	    views.add(breakpointProfileEditor);
	    livingViews.add(breakpointProfileEditor);
        varNameToView.put("breakpointProfile", breakpointProfileEditor);
		
		addControlButton("Control", "gui/icons/creep.png", "creep", "(C)ontinue to next step", KeyEvent.VK_C);
		addControlButton("Control", "gui/icons/roundskip.png", "auto-complete", "(A)uto-complete this step", KeyEvent.VK_A);
		//addControlButton("Control", "gui/icons/pause.png", "(un)pause", "(P)ause the current skip operation", KeyEvent.VK_P);
		addControlButton("Control", "gui/icons/skip.png", "skip", "(S)kip this step", KeyEvent.VK_S);
		addControlButton("Control", "gui/icons/reject.png", "fail", "make this step (F)ail", KeyEvent.VK_F);
		addControlButton("Control", "gui/icons/leap.png", "leap", "(L)eap to next breakpoint match", KeyEvent.VK_L);
		addControlButton("Control", "gui/icons/stop.png", "stop", "abort skip or leap (X)", KeyEvent.VK_X);
		
		addControlButton("History", "gui/icons/back.png", "backInHistory", "Back (Q)", KeyEvent.VK_Q);
		addControlButton("History", "gui/icons/forward.png", "forwardInHistory", "Forward (W)", KeyEvent.VK_W);

		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.CALL, Color.WHITE);
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.EXIT, new Color(153, 255, 102));
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.DET_EXIT, new Color(102, 153, 102));
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.FAIL, new Color(183, 50, 50));
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.EXCEPTION, new Color(204, 0, 153));
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.REDO, new Color(220, 110, 0));
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.PSEUDO_BLOCKED, Color.GRAY);
		mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.PSEUDO_UNBLOCKED, Color.LIGHT_GRAY);
	}
	
	protected KahinaAbstractTreeView generateTreeView()
	{
		return new KahinaListTreeView(kahina, 0);
	}

	/*protected KahinaLayeredTreeView generateTreeView(KahinaController control)
	{
		return new KahinaLayeredTreeView(true, control, 0);
	}*/

	@Override
	public void displayMainViews()
	{
		if (VERBOSE)
		{
			System.err.println(this + ".displayMainViews()");
		}
		LogicProgrammingState state = (LogicProgrammingState) kahina.getState();
		if (VERBOSE)
		{
			System.err.println("Displaying tree...");
		}
		mainTreeView.display(state.getStepTree());
		if (VERBOSE)
		{
			System.err.println("Displaying secondary tree...");
		}
		mainTreeView.displaySecondaryTree(state.getSecondaryStepTree());
		if (VERBOSE)
		{
			System.err.println("Displaying console messages...");
		}
		messageConsoleView.display(state.getConsoleMessages());
		if (VERBOSE)
		{
			System.err.println("//" + this + ".displayMainViews()");
		}
	}

	@Override
	protected void processDialogEvent(KahinaDialogEvent e)
	{
		super.processDialogEvent(e);
		switch (e.getDialogEventType())
		{
			case KahinaDialogEvent.PRIMARY_BREAKPOINTS:
			{
				if (VERBOSE)
				{
					System.err.println(this + " received primary breakpoints event.");
				}
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaDefaultInstance(), KahinaBreakpointType.PRIMARY_BREAKPOINT);
				breakpointEditor.setTitle("Edit search tree breakpoints");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getPrimaryBreakpoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.SECONDARY_BREAKPOINTS:
			{
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaDefaultInstance(), KahinaBreakpointType.SECONDARY_BREAKPOINT);
				breakpointEditor.setTitle("Edit call tree breakpoints");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getSecondaryBreakpoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.PRIMARY_WARN_POINTS:
			{
				if (VERBOSE)
				{
					System.err.println(this + " received primary warnpoints event.");
				}
				ThresholdedBreakpointEditorWindow breakpointEditor = new ThresholdedBreakpointEditorWindow(new KahinaDefaultInstance(), KahinaBreakpointType.PRIMARY_WARN_POINT);
				breakpointEditor.setTitle("Edit search tree warn points");
				LogicProgrammingState state = (LogicProgrammingState) kahina.getState();
				breakpointEditor.loadBreakpointProfile(state.getPrimaryWarnPoints());
				breakpointEditor.loadState(state);
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.SECONDARY_WARN_POINTS:
			{
				ThresholdedBreakpointEditorWindow breakpointEditor = new ThresholdedBreakpointEditorWindow(new KahinaDefaultInstance(), KahinaBreakpointType.SECONDARY_WARN_POINT);
				breakpointEditor.setTitle("Edit call tree warn points");
				LogicProgrammingState state = (LogicProgrammingState) kahina.getState();
				breakpointEditor.loadBreakpointProfile(state.getSecondaryWarnPoints());
				breakpointEditor.loadState(state);
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.SKIP_POINTS:
			{
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaDefaultInstance(), KahinaBreakpointType.SKIP_POINT);
				breakpointEditor.setTitle("Edit skip points");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getSkipPoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.CREEP_POINTS:
			{
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaDefaultInstance(), KahinaBreakpointType.CREEP_POINT);
				breakpointEditor.setTitle("Edit creep points");
				breakpointEditor.loadBreakpointProfile(((LogicProgrammingState) kahina.getState()).getCreepPoints());
				breakpointEditor.setVisible(true);
				break;
			}
			case KahinaDialogEvent.FAIL_POINTS:
			{
				BreakpointEditorWindow breakpointEditor = new BreakpointEditorWindow(new KahinaDefaultInstance(), KahinaBreakpointType.FAIL_POINT);
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
				int stepID = state.getSelectedStepID();
				JFrame window = new LogicProgrammingProfileWindow(((LogicProgrammingInstance<?, ?, ?>) kahina).getProfiler().profileSubtree(state.getSecondaryStepTree(), state.getStepTree(),
						stepID));
				window.setTitle("Profile of call subtree at " + ((LogicProgrammingState) kahina.getState()).get(stepID));
				window.setVisible(true);
				break;
			}
			case KahinaDialogEvent.SEARCH_SUBTREE_PROFILE:
			{
				LogicProgrammingState state = (LogicProgrammingState) kahina.getState();
				int stepID = state.getSelectedStepID();
				JFrame window = new LogicProgrammingProfileWindow(((LogicProgrammingInstance<?, ?, ?>) kahina).getProfiler().profileSubtree(state.getStepTree(), state.getStepTree(),
						stepID));
				window.setTitle("Profile of search subtree at " + ((LogicProgrammingState) kahina.getState()).get(stepID));
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
