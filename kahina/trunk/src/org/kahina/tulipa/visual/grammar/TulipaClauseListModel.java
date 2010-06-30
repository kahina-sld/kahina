package org.kahina.tulipa.visual.grammar;

import javax.swing.AbstractListModel;

import org.kahina.tulipa.data.grammar.TulipaGrammar;

public class TulipaClauseListModel extends AbstractListModel
{
	TulipaGrammar grammar;
	
	public TulipaClauseListModel()
	{
		grammar = new TulipaGrammar();
	}

	@Override
	public Object getElementAt(int arg0) 
	{
		return grammar.getClause(arg0);
	}

	@Override
	public int getSize() 
	{
		return grammar.getSize();
	}
}
