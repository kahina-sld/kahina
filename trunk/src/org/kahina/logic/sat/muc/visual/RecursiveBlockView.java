package org.kahina.logic.sat.muc.visual;

import java.awt.Color;
import java.util.HashMap;
import java.util.List;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.data.RecursiveBlockHandler;

public class RecursiveBlockView extends KahinaTreeView
{
    RecursiveBlockHandler blockHandler;
    
    public RecursiveBlockView(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        // TODO Auto-generated constructor stub
    }

    @Override
    public JComponent makePanel()
    {
        RecursiveBlockViewPanel panel = new RecursiveBlockViewPanel(kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
    
    public void display(RecursiveBlockHandler blockHandler)
    {
        this.blockHandler = blockHandler;
        recalculate();
    }
    
    public void recalculate()
    {
        //treeLayer = 0;
        model = blockHandler.retrieveBlockTree();
        //nodeBorderColor = new HashMap<Integer, Color>();
        super.recalculate();
        /*int stepID = kahina.getState().getSelectedStepID();
        listModel.clear();
        lineStatus.clear();
        if (stepID == -1)
        {
            displayText("No reduction state selected!");
        }
        else if (model.getBlocks().size() == 0)
        {
            displayText("No reduction blocks found so far!");
            lineStatus.add(0);
        }
        else
        {
            currentStep = kahina.getState().retrieve(MUCStep.class, stepID);
            for (List<Integer> block : model.retrieveBlocks())
            {
                StringBuilder s = new StringBuilder();
                s.append("[");
                for (Integer literal : block)
                {
                    s.append(literal);
                    s.append(',');
                }
                s.deleteCharAt(s.length() - 1);
                s.append(']');
                listModel.addElement(s.toString());
                lineStatus.add(currentStep.relationToBlock(block));
            }
        }*/
    }

}
