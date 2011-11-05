package org.kahina.qtype.data.bindings;

import org.kahina.core.data.KahinaObject;
import org.kahina.tralesld.data.fs.TraleSLDFS;

public class QTypeGoal extends KahinaObject
{
	private static final long serialVersionUID = 1626803422982352323L;

	private TraleSLDFS in;
	
	private TraleSLDFS out;
	
	public QTypeGoal()
	{
	}
	
	public QTypeGoal(QTypeGoal original)
	{
		in = original.in;
		out = original.out;
	}
	
	public void setIn(TraleSLDFS grisu)
	{
		in = grisu;
	}
	
	public void setOut(TraleSLDFS grisu)
	{
		out = grisu;
	}
	
	public TraleSLDFS getIn()
	{
		return in;
	}
	
	public TraleSLDFS getOut()
	{
		return out;
	}
}
