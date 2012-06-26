package org.kahina.qtype.bridge;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kahina.core.control.KahinaControlEvent;
import org.kahina.prolog.util.PrologUtil;
import org.kahina.qtype.QTypeDebuggerInstance;
import org.kahina.qtype.QTypeStep;
import org.kahina.qtype.control.QTypeControlEventCommands;
import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.sicstus.bridge.SICStusPrologBridge;
import org.kahina.tralesld.data.fs.TraleSLDFSPacker;

public class QTypeBridge extends SICStusPrologBridge
{
	
	private static final boolean VERBOSE = false;

	private static final Pattern SENTENCE_PATTERN = Pattern.compile("\\[([^\\]]+)\\]");

	private final TraleSLDFSPacker packer;

	public QTypeBridge(QTypeDebuggerInstance kahina)
	{
		super(kahina);
		packer = new TraleSLDFSPacker();
	}

	@Override
	public void step(int extID, String type, String description, String consoleMessage)
	{
		super.step(extID, type, description, consoleMessage);
		
		// For compile_grammar steps, unregister any previously registered grammar:
		if (description.startsWith("compile_grammar(") && description.endsWith(")"))
		{
			kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_GRAMMAR, new Object[] { null }));
		}
		
		// For lc steps, register the sentence:
		if (description.startsWith("lc(") && description.endsWith(")"))
		{
			Matcher matcher = SENTENCE_PATTERN.matcher(description.substring(3, description.length() - 1));

			if (matcher.matches())
			{
				kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_SENTENCE, new Object[] { PrologUtil.parsePrologStringList(matcher.group(0)) }));
			}
		}
	}
	
	@Override
	public void exit(int extID, boolean deterministic, String newDescription)
	{		
		super.exit(extID, deterministic, newDescription);
		
		// At exit of compile_grammar steps, register the grammar:
		if (newDescription.startsWith("compile_grammar(") && newDescription.endsWith(")"))
		{
			String path = PrologUtil.atomLiteralToString(newDescription.substring(16, newDescription.length() - 1));
			kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_GRAMMAR, new Object[] { path }));
		}
	}

	public void registerExample(int number, String expectation, String sentence)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".registerExample(" + number + ", " + expectation + ", " + sentence + ")");
		}
		try
		{
			kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_EXAMPLE, new Object[] { number, expectation, PrologUtil.parsePrologStringList(sentence) }));
		}
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}

	@Override
	protected QTypeStep generateStep()
	{
		return new QTypeStep();
	}

	public void registerGoal(int extID, String key, String grisu)
	{
		try
		{
			QTypeGoal goal = state.retrieve(QTypeStep.class, stepIDConv.get(extID)).getGoal();

			if ("in".equals(key))
			{
				goal.setIn(packer.pack(grisu));
			} else if ("out".equals(key))
			{
				goal.setOut(packer.pack(grisu));
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}

}
