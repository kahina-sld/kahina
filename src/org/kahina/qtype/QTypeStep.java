package org.kahina.qtype;

import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.sicstus.SICStusPrologStep;

public class QTypeStep extends SICStusPrologStep
{

	private static final long serialVersionUID = -119683692028732745L;
	
	public final QTypeGoal goal;
	
	public QTypeStep()
	{
		goal = new QTypeGoal();
	}
	
	public QTypeStep(QTypeStep original)
	{
		super(original);
		goal = new QTypeGoal(original.goal);
	}
	
	public QTypeGoal getGoal()
	{
		return goal;
	}
	
	@Override
	public QTypeStep copy()
	{
		return new QTypeStep(this);
	}

}
 