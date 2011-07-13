package org.kahina.core.visual.tree;

import java.util.HashSet;
import java.util.Set;

import org.kahina.core.data.tree.KahinaTree;

public class KahinaTreeViewMarker
{
    private KahinaTree model;
    
    private KahinaTree secondaryModel;
    
    private Set<KahinaTreeViewPanel> views;
    
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
    
    public void setModel(KahinaTree model)
    {
    	this.model = model;
    }
    
    public void setSecondaryModel(KahinaTree secondaryModel)
    {
    	this.secondaryModel = secondaryModel;
    }
    
    public void markNode(int nodeID)
    {     
        if (nodeID == -1)
        {
            //deactivate marking in all registered views, inconsistencies otherwise
            for (KahinaTreeViewPanel view : views)
            {
                view.view.setMarkedNode(-1);
                view.updateDisplayAndRepaintFromEventDispatchThread();
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
