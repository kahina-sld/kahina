package org.kahina.qtype.bridge;

import org.kahina.lp.LogicProgrammingState;
import org.kahina.qtype.QTypeStep;
import org.kahina.sicstus.bridge.SICStusPrologBridge;
import org.kahina.tralesld.data.fs.TraleSLDFSPacker;

public class QTypeBridge extends SICStusPrologBridge
{
	
	private final TraleSLDFSPacker packer;
	
	// TODO receive goals in Grisu format and store them in the step

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

}
