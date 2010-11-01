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
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaEdgeSelectionEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.interfaces.KahinaPrologInterface;
import org.kahina.core.interfaces.KahinaPrologInterfaceFactory;
import org.kahina.core.util.PrologUtilities;
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
	
	public static final Action COMPILE_ACTION = new AbstractAction("Compile")
	{
		
		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.COMPILE));
		}
		
	};
	
	public static final Action PARSE_ACTION = new AbstractAction("Parse")
	{
		
		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.PARSE));
		}
		
	};
	
	public static final Action RESTART_ACTION = new AbstractAction("Restart")
	{
		
		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.RESTART));
		}
		
	};

	private final KahinaPrologInterface prologInterface;

	private TraleSLDProfiler profiler;

	private String grammar;

	private List<String> sentence;

	public TraleSLDInstance()
	{
		prologInterface = KahinaPrologInterfaceFactory.create();
	}

	@Override
	public TraleSLDBridge startNewSession()
	{
		super.startNewSession();
		profiler = new TraleSLDProfiler(state.getFullProfile());
		controller.registerListener("edge select", this);
		controller.registerListener("update", this);

		if (prologInterface != null)
		{
			controller.registerListener("control", this);
		}

		return bridge;
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
		}
	}

	private void processControlEvent(KahinaControlEvent event)
	{
		String command = event.getCommand();

		if (TraleSLDControlEventCommands.REGISTER_SENTENCE.equals(command))
		{
			sentence = PrologUtilities.parsePrologStringList(event.getArguments()[0]);
		} else if (TraleSLDControlEventCommands.COMPILE.equals(command))
		{
			// TODO
		} else if (TraleSLDControlEventCommands.PARSE.equals(command))
		{
			// TODO
		} else if (TraleSLDControlEventCommands.RESTART.equals(command))
		{
			// TODO
		}
	}

	public void registerGrammar(String absolutePath)
	{
		grammar = absolutePath;
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
