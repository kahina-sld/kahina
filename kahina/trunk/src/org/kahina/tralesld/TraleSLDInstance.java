package org.kahina.tralesld;

import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.kahina.core.KahinaRunner;
import org.kahina.core.LogicProgrammingInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaSystemEvent;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaEdgeSelectionEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.util.PrologUtilities;
import org.kahina.core.util.Utilities;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.lp.visual.source.PrologJEditSourceCodeView;
import org.kahina.tralesld.behavior.TraleSLDTreeBehavior;
import org.kahina.tralesld.bridge.TraleSLDBridge;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;
import org.kahina.tralesld.gui.TraleSLDGUI;
import org.kahina.tralesld.profiler.TraleSLDProfiler;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;
import org.kahina.tralesld.visual.fs.TraleSLDVariableBindingSetView;
import org.tralesld.core.event.TraleSLDControlEventCommands;

public class TraleSLDInstance extends LogicProgrammingInstance<TraleSLDState, TraleSLDGUI, TraleSLDBridge>
{

	private static final boolean VERBOSE = false;

	private String traleCommand = "";
	
	private boolean commanding = false;

	// TODO disable if there's no Prolog interface
	public final Action COMPILE_ACTION = new AbstractAction("Compile")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.COMPILE));
		}

	};

	public final Action PARSE_ACTION = new AbstractAction("Parse")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.PARSE));
		}

	};

	public final Action RESTART_ACTION = new AbstractAction("Restart parse")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.RESTART));
		}

	};

	private TraleSLDProfiler profiler;

	private String grammar;

	private List<String> sentence;

	public TraleSLDInstance()
	{
		COMPILE_ACTION.setEnabled(false);
		PARSE_ACTION.setEnabled(false); // need grammar first
		RESTART_ACTION.setEnabled(false); // need grammar and sentence first
	}

	@Override
	public TraleSLDBridge startNewSession()
	{
		super.startNewSession();
		profiler = new TraleSLDProfiler(state.getFullProfile());
		controller.registerListener("edge select", this);
		controller.registerListener("update", this);
		controller.registerListener(KahinaEventTypes.SYSTEM, this);
		controller.registerListener(KahinaEventTypes.CONTROL, this);
		return bridge;
	}
	
	public synchronized String getCommand()
	{
		setCommanding(!"quit".equals(traleCommand));
		String result = traleCommand;
		traleCommand = "";
		return result;
	}
	
	private void setCommanding(boolean commanding)
	{
		if (!this.commanding && commanding)
		{
			// TODO show compile? parse? dialog
		}
		
		this.commanding = commanding;
		COMPILE_ACTION.setEnabled(commanding);
		PARSE_ACTION.setEnabled(commanding && grammar != null);
		RESTART_ACTION.setEnabled(commanding && grammar != null && sentence != null);
	}

	@Override
	protected void createTreeBehavior()
	{
		new TraleSLDTreeBehavior(state.getStepTree(), this, state.getSecondaryStepTree());
	}

	@Override
	protected TraleSLDBridge createBridge()
	{
		return new TraleSLDBridge(this.state);
	}

	@Override
	protected TraleSLDGUI createGUI(KahinaController guiController)
	{
		return new TraleSLDGUI(TraleSLDStep.class, this, guiController);
	}

	@Override
	protected TraleSLDState createState()
	{
		return new TraleSLDState();
	}

	@Override
	protected void fillViewRegistry()
	{
		super.fillViewRegistry();
		KahinaViewRegistry.registerMapping(TraleSLDFS.class, TraleSLDFeatureStructureView.class);
		KahinaViewRegistry.registerMapping(TraleSLDVariableBindingSet.class, TraleSLDVariableBindingSetView.class);
		KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, PrologJEditSourceCodeView.class);
	}

	@Override
	public void processEvent(KahinaEvent e)
	{
		super.processEvent(e);
		if (e instanceof KahinaEdgeSelectionEvent)
		{
			processEdgeSelectionEvent((KahinaEdgeSelectionEvent) e);
		} else if (e instanceof KahinaUpdateEvent)
		{
			processUpdateEvent((KahinaUpdateEvent) e);
		} else if (e instanceof KahinaControlEvent)
		{
			processControlEvent((KahinaControlEvent) e);
		} else if (e instanceof KahinaSystemEvent)
		{
			processSystemEvent((KahinaSystemEvent) e);
		}
	}

	private void processSystemEvent(KahinaSystemEvent e)
	{
		if (e.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			traleCommand = "quit";
		}
	}

	private synchronized void processControlEvent(KahinaControlEvent event)
	{
		String command = event.getCommand();

		if (TraleSLDControlEventCommands.REGISTER_SENTENCE.equals(command))
		{
			sentence = castToStringList(event.getArguments()[0]);
			if (grammar != null)
			{
				RESTART_ACTION.setEnabled(true);
			}
		} else if (TraleSLDControlEventCommands.REGISTER_GRAMMAR.equals(command))
		{
			grammar = (String) event.getArguments()[0];
			PARSE_ACTION.setEnabled(commanding);
			if (sentence != null)
			{
				RESTART_ACTION.setEnabled(true);
			}
		} else if (TraleSLDControlEventCommands.COMPILE.equals(command))
		{
			if (event.getArguments() == null || event.getArguments().length == 0)
			{
				KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.COMPILE, new Object[] { grammar }));
			} else
			{
				// HACK: set bridge to abort - if we go through the controller,
				// KahinaRunner will deinitialize and thwart subsequent eventing
				bridge.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
				compile((String) event.getArguments()[0]);
			}
		} else if (TraleSLDControlEventCommands.PARSE.equals(command))
		{
			if (event.getArguments() == null || event.getArguments().length == 0)
			{
				KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PARSE, new Object[] { sentence }));
			} else
			{
				// HACK: see above
				bridge.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
				parse(castToStringList(event.getArguments()[0]));
			}
		} else if (commanding && TraleSLDControlEventCommands.RESTART.equals(command))
		{
			// HACK: see above
			bridge.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
			compile(grammar);
			parse(sentence);
		}
	}

	@SuppressWarnings("unchecked")
	private List<String> castToStringList(Object object)
	{
		return (List<String>) object;
	}

	protected void compile(String absolutePath)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".compile(" + absolutePath + ")");
		}
		traleCommand = "query dcompile_gram(" + PrologUtilities.stringToAtomLiteral(absolutePath) + ").";
	}

	protected void parse(List<String> words)
	{
		traleCommand = "query drec[" + Utilities.join(",", words) + "].";
	}

	private void processEdgeSelectionEvent(KahinaEdgeSelectionEvent e)
	{
		int nodeID = state.getNodeForEdge(e.getSelectedEdge());
		if (nodeID != -1)
		{
			KahinaRunner.processEvent(new KahinaSelectionEvent(nodeID));
		}
	}

	private void processUpdateEvent(KahinaUpdateEvent e)
	{
		int edgeID = state.getEdgeForNode(e.getSelectedStep());
		if (edgeID != -1)
		{
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(edgeID));
		}
	}

	@Override
	public LogicProgrammingProfiler getProfiler()
	{
		return profiler;
	}

	public static void main(String[] args)
	{
		(new TraleSLDInstance()).start(args);
	}

}
