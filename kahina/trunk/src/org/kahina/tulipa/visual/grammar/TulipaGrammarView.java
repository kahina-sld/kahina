package org.kahina.tulipa.visual.grammar;

import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaRunner;
import org.kahina.core.visual.KahinaView;
import org.kahina.tulipa.data.grammar.TulipaGrammar;

public class TulipaGrammarView extends KahinaView<TulipaGrammar>
{
	int selectedClause;
	
    public JComponent wrapInPanel()
    {
        TulipaGrammarViewPanel panel = new TulipaGrammarViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
    
    public List<Integer> getSelectedClauses()
    {
    	ArrayList<Integer> selectedClauses = new ArrayList<Integer>();
    	if (selectedClause != -1)
    	{
    		selectedClauses.add(selectedClause);
    	}
    	return getSelectedClauses();
    }
}
