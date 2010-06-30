package org.kahina.tulipa.visual.grammar;

import javax.swing.AbstractListModel;

import org.kahina.tulipa.data.grammar.TulipaGrammar;

public class TulipaClauseListModel extends AbstractListModel
{
	private static final long serialVersionUID = -5222437359574027115L;
	
	TulipaGrammar grammar;
	
	public TulipaClauseListModel()
	{
		grammar = new TulipaGrammar();
	}
	
	public void setGrammar(TulipaGrammar grammar)
	{
		this.grammar = grammar;
	}

	@Override
	public Object getElementAt(int arg0) 
	{
		return grammar.getClause(arg0);
	}

	@Override
	public int getSize() 
	{
		System.err.println("Grammar size: " + grammar.getSize());
		return grammar.getSize();
	}
}
