package org.kahina.tralesld;

import java.awt.event.ActionEvent;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.List;
import java.util.Queue;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.kahina.core.KahinaRunner;
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
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.lp.visual.source.PrologJEditSourceCodeView;
import org.kahina.tralesld.behavior.TraleSLDTreeBehavior;
import org.kahina.tralesld.bridge.TraleSLDBridge;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;
import org.kahina.tralesld.event.TraleSLDControlEventCommands;
import org.kahina.tralesld.gui.TraleSLDGUI;
import org.kahina.tralesld.profiler.TraleSLDProfiler;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;
import org.kahina.tralesld.visual.fs.TraleSLDVariableBindingSetView;

public class TraleSLDInstance extends LogicProgrammingInstance<TraleSLDState, TraleSLDGUI, TraleSLDBridge>
{

	private static final boolean VERBOSE = false;

	Queue<String> traleCommands = new ArrayDeque<String>();

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

	private List<String> sentence = Collections.emptyList();

	public TraleSLDInstance()
	{
		COMPILE_ACTION.setEnabled(false);
		PARSE_ACTION.setEnabled(false); // need grammar first
		RESTART_ACTION.setEnabled(false); // need grammar and sentence first
	}

	@Override
	public TraleSLDBridge startNewSession()
	{
		try
		{
			super.startNewSession();
			profiler = new TraleSLDProfiler(state.getFullProfile());
			controller.registerListener("edge select", this);
			controller.registerListener("update", this);
			controller.registerListener(KahinaEventTypes.CONTROL, this);
			return bridge;
		}
		catch (NullPointerException e)
		{
			System.err.println("NULL POINTER EXCEPTION at the following stack position:");
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}

	public String getCommand()
	{
		synchronized (traleCommands)
		{
			if (traleCommands.isEmpty())
			{
				commanding = true;
				updateActions();
				return "";
			}

			String traleCommand = traleCommands.remove();
			commanding = !"quit".equals(traleCommand);
			updateActions();
			if (VERBOSE)
			{
				System.err.println(this + ".getCommand()=" + traleCommand + "(Queue: " + traleCommands + ")");
			}
			return traleCommand;
		}
	}

	private void updateActions()
	{
		COMPILE_ACTION.setEnabled(commanding);
		PARSE_ACTION.setEnabled(commanding && grammar != null);
		RESTART_ACTION.setEnabled(commanding && grammar != null && !sentence.isEmpty());
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
			synchronized (traleCommands)
			{
				if (commanding)
				{
					traleCommands.add("quit");
				}
			}
		}
	}

	private void processControlEvent(KahinaControlEvent event)
	{
		String command = event.getCommand();

		if (TraleSLDControlEventCommands.REGISTER_SENTENCE.equals(command))
		{
			sentence = castToStringList(event.getArguments()[0]);
			updateActions();
			if (VERBOSE)
			{
				System.err.println("Sentence registered.");
			}
		} 
		else if (TraleSLDControlEventCommands.REGISTER_GRAMMAR.equals(command))
		{
			grammar = (String) event.getArguments()[0];
			PARSE_ACTION.setEnabled(commanding);
			updateActions();
			if (VERBOSE)
			{
				System.err.println("Grammar registered.");
			}
		} 
		else if (TraleSLDControlEventCommands.COMPILE.equals(command))
		{
			if (event.getArguments() == null || event.getArguments().length == 0)
			{
				KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.COMPILE, new Object[] { grammar }));
			} 
			else
			{
				// Lazy hack: set bridge to abort - if we go through the controller,
				// KahinaRunner will deinitialize and thwart subsequent eventing
				bridge.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
				compile((String) event.getArguments()[0]);
			}
		} 
		else if (TraleSLDControlEventCommands.PARSE.equals(command))
		{
			if (event.getArguments() == null || event.getArguments().length == 0)
			{
				KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PARSE, new Object[] { Utilities.join(" ", sentence) }));
			} 
			else
			{
				// Lazy hack: see above
				bridge.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
				parse(castToStringList(event.getArguments()[0]));
			}
		} 
		else if (TraleSLDControlEventCommands.RESTART.equals(command))
		{
			// Lazy hack: see above
			bridge.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
			compile(grammar);
			parse(sentence);
		}
		else if (TraleSLDControlEventCommands.REBUILD_SIGNATURE_INFO.equals(command))
		{
			gui.signatureUpdate();
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
		synchronized (traleCommands)
		{
			traleCommands.add("query dcompile_gram(" + PrologUtilities.stringToAtomLiteral(absolutePath) + ").");
			traleCommands.add("query send_signature.");
		}
	}

	protected void parse(List<String> words)
	{
		synchronized (traleCommands)
		{
			traleCommands.add("query drec[" + Utilities.join(",", words) + "].");
		}
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
