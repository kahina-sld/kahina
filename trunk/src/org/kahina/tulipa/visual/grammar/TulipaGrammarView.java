package org.kahina.tulipa.visual.grammar;

import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.tulipa.data.grammar.TulipaGrammar;

public class TulipaGrammarView extends KahinaView<TulipaGrammar>
{
	public TulipaGrammarView(KahinaController control)
	{
		super(control);
	}

	int selectedClause;
	
    @Override
	public JComponent makePanel(KahinaGUI gui)
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
