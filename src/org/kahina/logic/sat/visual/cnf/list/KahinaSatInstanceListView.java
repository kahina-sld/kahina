package org.kahina.logic.sat.visual.cnf.list;

import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.ListModel;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaView;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class KahinaSatInstanceListView extends KahinaView<CnfSatInstance>
{
    protected DefaultListModel listModel;
    
    public KahinaSatInstanceListView(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        listModel = new DefaultListModel();
    }
    
    public void doDisplay()
    {
        listModel.clear();
        recalculate();
    }
    
    public ListModel getListModel()
    {
        return listModel;
    }

    @Override
    public JComponent makePanel()
    {
        KahinaSatInstanceListViewPanel panel = new KahinaSatInstanceListViewPanel(kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }

    public void displayText(String string)
    {
        listModel = new DefaultListModel();
        listModel.addElement(string);
    }
    
    public void recalculate()
    {
        List<List<Integer>> clauses = model.getClauses();
        if (clauses.size() != listModel.getSize())
        {
            listModel.clear();
            for (int i = 1; i < clauses.size(); i++)
            {
                StringBuilder s = new StringBuilder();
                s.append(i);
                s.append(": {");
                for (Integer literal : clauses.get(i))
                {
                    s.append(literal);
                    s.append(',');
                }
                s.deleteCharAt(s.length() - 1);
                s.append('}');
                listModel.addElement(s.toString());
            }
        }
    }
}
