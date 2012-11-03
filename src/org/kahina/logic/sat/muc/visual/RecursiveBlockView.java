package org.kahina.logic.sat.muc.visual;

import java.util.LinkedList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.data.RecursiveBlockHandler;

public class RecursiveBlockView extends KahinaTreeView
{
    RecursiveBlockHandler blockHandler;
    MUCInstance kahina;
    
    public RecursiveBlockView(MUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
    }
    
    public RecursiveBlockHandler getBlockHandler()
    {
        return blockHandler;
    }

    @Override
    public JComponent makePanel()
    {
        RecursiveBlockViewPanel panel = new RecursiveBlockViewPanel(kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        JScrollPane scrollPane = new JScrollPane(panel);
        scrollPane.getViewport().setBackground(config.getBackgroundColor());
        return scrollPane;
    }
    
    public void display(RecursiveBlockHandler blockHandler)
    {
        this.blockHandler = blockHandler;
        recalculate();
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaSelectionEvent)
        {
            processEvent((KahinaSelectionEvent) e);
        }
    }
    
    public void processEvent(KahinaSelectionEvent e)
    {
        if (model != null) 
        {
            recalculate();
            kahina.dispatchEvent(new KahinaRedrawEvent());
        }
    }
    
    public void recalculate()
    {
        //treeLayer = 0;
        if (blockHandler == null) return;
        model = blockHandler.retrieveBlockTree();
        //update node status in model
        int stepID = kahina.getState().getSelectedStepID();
        if (stepID != -1)
        {
            MUCStep currentStep = kahina.getState().retrieve(MUCStep.class, stepID);
            List<Integer> nodeAgenda = new LinkedList<Integer>();
            nodeAgenda.add(model.getRootID());
            while (nodeAgenda.size() > 0)
            {
                int blockID = nodeAgenda.remove(0);
                List<Integer> block = blockHandler.getBlock(blockID);
                model.setNodeStatus(blockID, currentStep.relationToBlock(block));
                nodeAgenda.addAll(blockHandler.getSubblocks(blockID));
            }
            //nodeBorderColor = new HashMap<Integer, Color>();
            super.recalculate();
        }
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
