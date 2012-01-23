package org.kahina.qtype;

import java.awt.event.ActionEvent;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Queue;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.gui.KahinaDialogEvent;
import org.kahina.core.util.PrologUtilities;
import org.kahina.core.util.Utilities;
import org.kahina.qtype.control.QTypeControlEventCommands;

public class QTypeCommander implements KahinaListener
{

	private static final boolean VERBOSE = false;

	private Queue<String> commands = new ArrayDeque<String>();

	private boolean commanding = false;

	private String grammar;

	private List<String> sentence = Collections.emptyList();

	private List<List<String>> examples = new ArrayList<List<String>>();

	public final Action COMPILE_ACTION = new AbstractAction("Compile...")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(QTypeControlEventCommands.COMPILE));
		}

	};

	public final Action PARSE_ACTION = new AbstractAction("Parse...")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(QTypeControlEventCommands.PARSE));
		}

	};

	public final Action RESTART_ACTION = new AbstractAction("Restart parse")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			KahinaRunner.processEvent(new KahinaControlEvent(QTypeControlEventCommands.RESTART));
		}

	};

	public String getCommand()
	{
		synchronized (commands)
		{
			if (commands.isEmpty())
			{
				commanding = true;
				updateActions();
				return "";
			}

			String command = commands.remove();
			commanding = !"quit".equals(command);
			updateActions();
			if (VERBOSE)
			{
				System.err.println(this + ".getCommand()=" + command + "(Queue: " + commands + ")");
			}
			return command;
		}
	}

	private void updateActions()
	{
		COMPILE_ACTION.setEnabled(commanding);
		PARSE_ACTION.setEnabled(commanding && grammar != null);
		RESTART_ACTION.setEnabled(commanding && grammar != null && !sentence.isEmpty());
	}

	@Override
	public void processEvent(KahinaEvent e)
	{
		if (e instanceof KahinaControlEvent)
		{
			processControlEvent((KahinaControlEvent) e);
		} else if (e instanceof KahinaSystemEvent)
		{
			processSystemEvent((KahinaSystemEvent) e);
		}
	}

	private void processSystemEvent(KahinaSystemEvent e)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".processSystemEvent(" + e + ")");
		}
		if (e.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			synchronized (commands)
			{
				if (commanding)
				{
					commands.add("quit");
				}
			}
		}
	}

	private void processControlEvent(KahinaControlEvent event)
	{
		String command = event.getCommand();

		if (QTypeControlEventCommands.REGISTER_SENTENCE.equals(command))
		{
			sentence = Utilities.castToStringList(event.getArguments()[0]);
			updateActions();
			if (VERBOSE)
			{
				System.err.println("Sentence registered.");
			}
		} else if (QTypeControlEventCommands.REGISTER_EXAMPLE.equals(command))
		{
			Object[] arguments = event.getArguments();
			int number = (Integer) arguments[0];
			Utilities.ensureSize(examples, number + 1);
			examples.set(number, Utilities.castToStringList(arguments[2]));
			KahinaRunner.processEvent(new KahinaControlEvent(QTypeControlEventCommands.UPDATE_EXAMPLES, new Object[] { examples }));
		} else if (QTypeControlEventCommands.REGISTER_GRAMMAR.equals(command))
		{
			grammar = (String) event.getArguments()[0];
			sentence = Collections.emptyList();
			examples = new ArrayList<List<String>>();
			KahinaRunner.processEvent(new KahinaControlEvent(QTypeControlEventCommands.UPDATE_EXAMPLES, new Object[] { examples }));
			updateActions();
			if (VERBOSE)
			{
				System.err.println("Grammar registered.");
			}
		} else if (QTypeControlEventCommands.COMPILE.equals(command))
		{
			if (event.getArguments() == null || event.getArguments().length == 0)
			{
				KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.COMPILE, new Object[] { grammar }));
			} else
			{
				KahinaRunner.processEvent(new KahinaControlEvent("abort"));
				compile((String) event.getArguments()[0]);
			}
		} else if (QTypeControlEventCommands.PARSE.equals(command))
		{
			if (event.getArguments() == null || event.getArguments().length == 0)
			{
				KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PARSE, new Object[] { Utilities.join(" ", sentence) }));
			} else
			{
				KahinaRunner.processEvent(new KahinaControlEvent("abort"));
				parse(Utilities.castToStringList(event.getArguments()[0]));
			}
		} else if (QTypeControlEventCommands.RESTART.equals(command))
		{
			KahinaRunner.processEvent(new KahinaControlEvent("abort"));
			compile(grammar);
			parse(sentence);
		}
	}

	protected void compile(String absolutePath)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".compile(" + absolutePath + ")");
		}
		synchronized (commands)
		{
			commands.add("query cp(" + PrologUtilities.stringToAtomLiteral(absolutePath) + ")");
		}
	}

	protected void parse(List<String> words)
	{
		synchronized (commands)
		{
			commands.add("query lc(" + Utilities.join(" ", words) + ")");
		}
	}

	public void initializeForNewSession()
	{
		KahinaRunner.getControl().registerListener(KahinaEventTypes.SYSTEM, this);
		KahinaRunner.getControl().registerListener(KahinaEventTypes.CONTROL, this);
	}

}
