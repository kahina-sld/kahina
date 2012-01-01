package org.kahina.qtype.bridge;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.util.PrologUtilities;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.qtype.QTypeStep;
import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.qtype.event.QTypeControlEventCommands;
import org.kahina.sicstus.bridge.SICStusPrologBridge;
import org.kahina.tralesld.data.fs.TraleSLDFSPacker;

public class QTypeBridge extends SICStusPrologBridge
{

	private final TraleSLDFSPacker packer;

	public QTypeBridge(LogicProgrammingState state)
	{
		super(state);
		packer = new TraleSLDFSPacker();
	}
	
	@Override
	public void step(int extID, String type, String description, String consoleMessage)
	{
		super.step(extID, type, description, consoleMessage);
		if (description.startsWith("compile_grammar(") && description.endsWith(")"))
		{
			String path = PrologUtilities.atomLiteralToString(description.substring(16, description.length() - 1));
			KahinaRunner.processEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_GRAMMAR, new Object[] { path }));
		} else if (description.startsWith("lc(") && description.endsWith(")")) {
			String sentence = description.substring(3, description.length() - 1);
			KahinaRunner.processEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_SENTENCE, new Object[] { PrologUtilities.parsePrologStringList(sentence) }));
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
			QTypeGoal goal = KahinaRunner.retrieve(QTypeStep.class, stepIDConv.get(extID)).getGoal();

			if ("in".equals(key))
			{
				goal.setIn(packer.pack(grisu));
			} else if ("out".equals(key))
			{
				goal.setOut(packer.pack(grisu));
			}
			
			selectIfPaused(convertStepID(extID));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}

}
