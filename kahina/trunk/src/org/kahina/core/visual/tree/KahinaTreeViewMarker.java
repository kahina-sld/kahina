package org.kahina.core.visual.tree;

import java.util.HashSet;
import java.util.Set;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.event.KahinaSelectionEvent;

public class KahinaTreeViewMarker
{
    int markedNode = -1;
    KahinaTree model;
    KahinaTree secondaryModel;
    Set<KahinaTreeViewPanel> views;
    
    public KahinaTreeViewMarker(KahinaTree m)
    {
        this.model = m;
        views = new HashSet<KahinaTreeViewPanel>();
    }
    
    public KahinaTreeViewMarker(KahinaTree m, KahinaTree m2)
    {
        this.model = m;
        this.secondaryModel = m2;
        views = new HashSet<KahinaTreeViewPanel>();
    }
    
    public void registerTreeView(KahinaTreeViewPanel view)
    {
        views.add(view);
    }
    
    public void markNode(int nodeID)
    {     
        if (nodeID == -1)
        {
            //deactivate marking in all registered views, inconsistencies otherwise
            for (KahinaTreeViewPanel view : views)
            {
                view.view.setMarkedNode(-1);
                view.updateDisplay();
                view.repaint();
            }
        }
        else
        {
            model.setReferenceNode(nodeID);
            if (secondaryModel != null)
            {
                secondaryModel.setReferenceNode(nodeID);
            }
            for (KahinaTreeViewPanel view : views)
            {
                view.view.recalculate(); // TODO often necessary so displaysNode(nodeID) will be true. Can we do this more efficiently?
                if (view.view.displaysNode(nodeID))
                {
                   view.view.setMarkedNode(nodeID);
                   view.scrollToNode(nodeID);
                }
                else
                {
                    int newNodeID = model.getParent(nodeID, view.view.getTreeLayer());
                    view.view.setMarkedNode(newNodeID);
                    if (newNodeID != -1)
                    {
                        view.scrollToNode(newNodeID);
                    }
                }
            }
        }
    }
}
