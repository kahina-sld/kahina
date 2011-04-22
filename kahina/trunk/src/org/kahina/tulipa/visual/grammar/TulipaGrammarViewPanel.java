package org.kahina.tulipa.visual.grammar;

import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.kahina.core.visual.KahinaViewPanel;

public class TulipaGrammarViewPanel extends KahinaViewPanel<TulipaGrammarView> implements ListSelectionListener
{
	private static final long serialVersionUID = 8545282386910165013L;
	
	private static final boolean VERBOSE = false;

	private final JList clauseList;
	
	private final JPanel innerPanel;
	
	private final TulipaClauseListModel clauseListModel = new TulipaClauseListModel();
	
	public TulipaGrammarViewPanel()
	{
		if (VERBOSE)
		{
			System.err.println("TulipaGrammarViewPanel" + this.hashCode() + "()");
		}
		setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		clauseList = new JList(clauseListModel);
		clauseList.getSelectionModel().addListSelectionListener(this);
		JScrollPane clauseListScrollPane = new JScrollPane(clauseList);
		JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		splitPane.add(clauseListScrollPane);
		innerPanel = new JPanel();
		splitPane.add(new JScrollPane(innerPanel));
		add(splitPane);
		if (VERBOSE)
		{
			System.err.println("//TulipaGrammarViewPanel" + this.hashCode() + "()");
		}
	}
	
    public void setView(TulipaGrammarView view)
    {
        this.view = view;
        //clauseList.setModel(view.getListModel());
        //clauseList.setSelectionModel(view.getSelectionModel());
        revalidate();
        updateDisplay();
    }
	
	@Override
	public void valueChanged(ListSelectionEvent e)
	{
		//first version: do not really do anything
	}
	
	@Override
	public void updateDisplay()
	{
		if (VERBOSE)
		{
			System.err.println("TulipaGrammarViewPanel" + this.hashCode() + ".updateDisplay()");
		}
		clauseListModel.setGrammar(view.getModel());
		List<Integer> newSelectedRows = view.getSelectedClauses();
		if (VERBOSE) System.err.println("newSelectedRows = " + newSelectedRows);
		ListSelectionModel selectionModel = clauseList.getSelectionModel();
		selectionModel.clearSelection();
		for (int rowIndex : newSelectedRows)
		{
			selectionModel.addSelectionInterval(rowIndex, rowIndex);
		}
		if (VERBOSE)
		{
			System.err.println("//TulipaGrammarViewPanel" + this.hashCode() + ".updateDisplay()");
		}
	}
}
