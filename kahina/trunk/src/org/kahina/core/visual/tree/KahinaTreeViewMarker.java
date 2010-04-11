package org.kahina.core.visual.tree;

import java.util.HashSet;
import java.util.Set;

import org.kahina.core.data.tree.KahinaTree;

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
    
    //TODO: turn this into a test case for the future "tentacle" concept
    //TODO: much more complex because of interaction between multiple views
    public void markNode(int nodeID)
    {
        if (nodeID == -1)
        {
            //deactivate marking in all registered views, inconsistencies otherwise
            for (KahinaTreeViewPanel view : views)
            {
                view.v.setMarkedNode(-1);
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
                view.v.resetAllStructures();
                view.v.calculateCoordinates();
                if (view.v.displaysNode(nodeID))
                {
                    view.v.setMarkedNode(nodeID);
                }
                else
                {
                    view.v.setMarkedNode(model.getParent(nodeID, view.v.getTreeLayer()));
                }
                view.updateDisplay();
                view.repaint();
            }
        }
    }
}
