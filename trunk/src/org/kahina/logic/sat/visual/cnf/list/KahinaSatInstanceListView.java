package org.kahina.logic.sat.visual.cnf.list;

import java.awt.Color;
import java.util.HashMap;
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
    
    // mapping from status values to display properties
    HashMap<Integer, Color> statusColorEncoding;
    
    public KahinaSatInstanceListView(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        listModel = new DefaultListModel();
        statusColorEncoding = new HashMap<Integer, Color>();
    }
    
    public void doDisplay()
    {
        listModel.clear();
        recalculate();
        needsRedraw = true;
    }
    
    public void setStatusColorEncoding(int status, Color color)
    {
        statusColorEncoding.put(status, color);
    }
    
    public int getLineStatus(int lineID)
    {
        return 0;
    }
    
    public Color getLineColor(int lineID)
    {
        int status = getLineStatus(lineID);
        Color col = statusColorEncoding.get(status);
        if (col == null)
        {
            return Color.BLACK;
        } 
        else
        {
            return col;
        }
    }
    
    public ListModel getListModel()
    {
        return listModel;
    }

    @Override
    public JComponent makePanel()
    {
        KahinaSatInstanceListViewPanel panel = new KahinaSatInstanceListViewPanel();
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }

    public void displayText(String string)
    {
        listModel.clear();
        listModel.addElement(string);
        needsRedraw = true;
    }
    
    public void recalculate()
    {
        kahina.getLogger().startMeasuring();
        if (model == null)
        {
            displayText("No model assigned yet.");
            return;
        }
        if (model.needsRebuild())
        {
            listModel.clear();
            for (int i = 0; i < model.getSize(); i++)
            {
                StringBuilder s = new StringBuilder();
                s.append(i + 1);
                s.append(": {");
                for (Integer literal : model.getClause(i))
                {
                    s.append(model.getSymbolForLiteral(literal));
                    s.append(',');
                }
                s.deleteCharAt(s.length() - 1);
                s.append('}');
                listModel.addElement(s.toString());
            }
        }
        else
        {
            //much easier case: no need to rebuild the entire list model
            if (model.getSize() > listModel.getSize())
            {
                for (int i = listModel.getSize(); i < model.getSize(); i++)
                {
                    StringBuilder s = new StringBuilder();
                    s.append(i + 1);
                    s.append(": {");
                    for (Integer literal : model.getClause(i))
                    {
                        s.append(model.getSymbolForLiteral(literal));
                        s.append(',');
                    }
                    s.deleteCharAt(s.length() - 1);
                    s.append('}');
                    listModel.addElement(s.toString());
                }
            }
        }
        needsRedraw = true;
        kahina.getLogger().endMeasuring("for recalculating " + this);
    }
    
    public boolean needsRedraw()
    {
        if (needsRedraw || model.needsUpdate())
        {
            needsRedraw = false;
            return true;
        }
        else 
        {
            return false;
        }
    }
    
    public void requireRedraw()
    {
        needsRedraw = true;
    }
}
