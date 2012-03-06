package org.kahina.qtype;

import java.awt.event.ActionEvent;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Queue;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;

import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.gui.KahinaDialogEvent;
import org.kahina.core.util.ListUtil;
import org.kahina.prolog.util.PrologUtil;
import org.kahina.qtype.control.QTypeControlEventCommands;
import org.kahina.qtype.gui.QTypeParseExampleMenu;

public class QTypeCommander implements KahinaListener
{
	private static final boolean VERBOSE = false;

	private Queue<String> commands = new ArrayDeque<String>();

	private boolean commanding = false;

	private String grammar;

	private List<String> sentence = Collections.emptyList();

	private List<List<String>> examples = new ArrayList<List<String>>();
	
	private KahinaController control;
	
	public QTypeCommander(KahinaController control)
	{
		this.control = control;
	}

	public final Action COMPILE_ACTION = new AbstractAction("Compile...")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			control.processEvent(new KahinaControlEvent(QTypeControlEventCommands.COMPILE));
		}

	};

	public final Action PARSE_ACTION = new AbstractAction("Parse...")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			control.processEvent(new KahinaControlEvent(QTypeControlEventCommands.PARSE));
		}

	};
	
	/**
	 * HACK This menu must be disabled/enabled by controller as appropriate,
	 * just like the actions. That's why we lifecycle-manage it here, even
	 * though it is a GUI element. We initialize it lazily because its
	 * constructor accesses the GUI. 
	 */
	private JMenu parseExampleMenu = null;
	
	public JMenu getParseExampleMenu()
	{
		if (parseExampleMenu == null)
		{
			parseExampleMenu = new QTypeParseExampleMenu(control);
		}
		
		return parseExampleMenu;
	}

	public final Action RESTART_ACTION = new AbstractAction("Restart parse")
	{

		private static final long serialVersionUID = -3829326193202814557L;

		@Override
		public void actionPerformed(ActionEvent e)
		{
			control.processEvent(new KahinaControlEvent(QTypeControlEventCommands.RESTART));
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
		parseExampleMenu.setEnabled(commanding && grammar != null);
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
			sentence = ListUtil.castToStringList(event.getArguments()[0]);
			updateActions();
			if (VERBOSE)
			{
				System.err.println("Sentence registered.");
			}
		} else if (QTypeControlEventCommands.REGISTER_EXAMPLE.equals(command))
		{
			Object[] arguments = event.getArguments();
			int number = (Integer) arguments[0];
			ListUtil.ensureSize(examples, number + 1);
			examples.set(number, ListUtil.castToStringList(arguments[2]));
			control.processEvent(new KahinaControlEvent(QTypeControlEventCommands.UPDATE_EXAMPLES, new Object[] { examples }));
		} else if (QTypeControlEventCommands.REGISTER_GRAMMAR.equals(command))
		{
			grammar = (String) event.getArguments()[0];
			sentence = Collections.emptyList();
			examples = new ArrayList<List<String>>();
			control.processEvent(new KahinaControlEvent(QTypeControlEventCommands.UPDATE_EXAMPLES, new Object[] { examples }));
			updateActions();
			if (VERBOSE)
			{
				System.err.println("Grammar registered.");
			}
		} else if (QTypeControlEventCommands.COMPILE.equals(command))
		{
			if (event.getArguments() == null || event.getArguments().length == 0)
			{
				control.processEvent(new KahinaDialogEvent(KahinaDialogEvent.COMPILE, new Object[] { grammar }));
			} else
			{
				control.processEvent(new KahinaControlEvent("abort"));
				compile((String) event.getArguments()[0]);
			}
		} else if (QTypeControlEventCommands.PARSE.equals(command))
		{
			if (event.getArguments() == null || event.getArguments().length == 0)
			{
				control.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PARSE, new Object[] { ListUtil.join(" ", sentence) }));
			} else
			{
				control.processEvent(new KahinaControlEvent("abort"));
				parse(ListUtil.castToStringList(event.getArguments()[0]));
			}
		} else if (QTypeControlEventCommands.RESTART.equals(command))
		{
			control.processEvent(new KahinaControlEvent("abort"));
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
			commands.add("query cp(" + PrologUtil.stringToAtomLiteral(absolutePath) + ")");
		}
	}

	protected void parse(List<String> words)
	{
		synchronized (commands)
		{
			commands.add("query lc(" + ListUtil.join(" ", words) + ")");
		}
	}

	public void initializeForNewSession()
	{
		control.registerListener(KahinaEventTypes.SYSTEM, this);
		control.registerListener(KahinaEventTypes.CONTROL, this);
	}

}
