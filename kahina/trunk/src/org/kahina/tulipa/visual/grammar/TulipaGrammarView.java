package org.kahina.tulipa.visual.grammar;

import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JComponent;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.text.KahinaTextView;
import org.kahina.tulipa.data.grammar.TulipaGrammar;

public class TulipaGrammarView extends KahinaView<TulipaGrammar>
{
	public TulipaGrammarView(KahinaController control)
	{
		super(control);
	}

	int selectedClause;
	
    @Override
	public JComponent wrapInPanel(KahinaController control)
    {
        TulipaGrammarViewPanel panel = new TulipaGrammarViewPanel();
        control.registerListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
    
    public List<Integer> getSelectedClauses()
    {
    	ArrayList<Integer> selectedClauses = new ArrayList<Integer>();
    	if (selectedClause != -1)
    	{
    		selectedClauses.add(selectedClause);
    	}
    	return selectedClauses;
    }
}
