package org.kahina.qtype.data.bindings;

import gralej.parsers.ParseException;

import org.kahina.core.data.KahinaObject;
import org.kahina.qtype.visual.fs.QTypeFSVisualizationUtility;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.visual.fs.FSVisualizationUtility;

public class QTypeGoal extends KahinaObject
{
	private static final long serialVersionUID = 1626803422982352323L;
	
	private static final boolean TEST_MODE = false;

	private TraleSLDFS fs;
	
	private TraleSLDFS tree;

	private TraleSLDFS in;
	
	private TraleSLDFS out;
	
	private static final FSVisualizationUtility util = new QTypeFSVisualizationUtility();
	
	public QTypeGoal()
	{
	}
	
	public QTypeGoal(QTypeGoal original)
	{
		this();
		fs = original.fs;
		tree = original.tree;
		in = original.in;
		out = original.out;
	}
	
	public void setFS(TraleSLDFS grisu)
	{
		test(grisu);
		fs = grisu;
	}
	
	public void setTree(TraleSLDFS grisu)
	{
		test(grisu);
		tree = grisu;
	}
	
	public void setIn(TraleSLDFS grisu)
	{
		test(grisu);
		in = grisu;
	}
	
	public void setOut(TraleSLDFS grisu)
	{
		test(grisu);
		out = grisu;
	}
	
	public TraleSLDFS getFS()
	{
		return fs;
	}
	
	public TraleSLDFS getTree()
	{
		return tree;
	}
	
	public TraleSLDFS getIn()
	{
		return in;
	}
	
	public TraleSLDFS getOut()
	{
		return out;
	}
	
	private void test(TraleSLDFS grisuMessage)
	{
		if (TEST_MODE)
		{
			String string = grisuMessage.toString();
			System.err.print(string);
			
			try
			{
				util.parseGrisu(string);
			} catch (ParseException e)
			{
				e.printStackTrace();
			}
		}
	}
}
