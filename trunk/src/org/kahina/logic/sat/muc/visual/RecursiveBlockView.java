package org.kahina.logic.sat.muc.visual;

import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
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
        else
        {
            super.processEvent(e);
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
            //compute the states bottom-up, relations only need to be computed for leaves
            List<Integer> nodeAgenda = new LinkedList<Integer>();
            //this relies on the following two facts:
            //    model.getAllNodeIDs() returns all node IDs in ascending order
            //    during block tree construction, child blocks have higher IDs than their parents
            nodeAgenda.addAll(model.getAllNodeIDs());
            System.err.println("nodeAgenda: " + nodeAgenda);
            while (nodeAgenda.size() > 0)
            {
                int blockID = nodeAgenda.remove(nodeAgenda.size() - 1);
                List<Integer> children = model.getChildren(blockID);
                if (children.size() == 0)
                {
                    TreeSet<Integer> block = blockHandler.getBlock(blockID);
                    model.setNodeStatus(blockID, currentStep.relationToBlock(block));
                }
                else
                {
                    int derivedStatus = 0;
                    int[] numStatusChildren = new int[4];
                    for (int childBlockID : children)
                    {
                        numStatusChildren[model.getNodeStatus(childBlockID)]++;
                    }
                    if (numStatusChildren[3] == children.size())
                    {
                        //all subblocks gray -> parent block gray
                        derivedStatus = 3;
                    }
                    else if (numStatusChildren[1] == children.size())
                    {
                        //all subblocks red -> parent block red
                        derivedStatus = 1;
                    }
                    else if (numStatusChildren[2] == children.size())
                    {
                        //all subblocks green -> parent block green
                        derivedStatus = 2;
                    }
                    else
                    {
                        //otherwise: node stays white (we don't know anything)
                    }
                    model.setNodeStatus(blockID, derivedStatus);
                }
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
    
    //revert the hack in KahinaTreeView, we do not want marking here!
    protected void processEvent(KahinaUpdateEvent e)
    {
        if (blockHandler != null && blockHandler.needsUpdate())
        {
            recalculate();
        }
    }
}
