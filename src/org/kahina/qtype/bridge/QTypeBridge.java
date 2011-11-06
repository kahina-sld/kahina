package org.kahina.qtype.bridge;

import org.kahina.core.KahinaRunner;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.qtype.QTypeStep;
import org.kahina.qtype.data.bindings.QTypeGoal;
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
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}

}
