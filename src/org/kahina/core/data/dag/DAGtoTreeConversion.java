package org.kahina.core.data.dag;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.core.data.tree.KahinaMemTree;

public class DAGtoTreeConversion
{
    public static KahinaMemTree forwardExpansionFromNode(KahinaDAG dag, int startID)
    {
        KahinaMemTree tree = new KahinaMemTree();
        Map<Integer,Integer> treeToTagID = new TreeMap<Integer,Integer>();
        int rootID = tree.addNode(dag.getNodeCaption(startID), "", dag.getNodeStatus(startID));
        treeToTagID.put(rootID, startID);
        tree.setRootID(rootID);
        List<Integer> expansionAgenda = new LinkedList<Integer>();
        expansionAgenda.add(rootID);
        while (expansionAgenda.size() > 0)
        {
            int currentID = expansionAgenda.remove(0);
            for (int edgeID : dag.getOutgoingEdges(treeToTagID.get(currentID)))
            {
                int endID = dag.getEndNode(edgeID);
                int childID = tree.addNode(dag.getNodeCaption(endID), "", dag.getNodeStatus(endID));
                treeToTagID.put(childID, endID);
                tree.addChild(currentID, childID);
                expansionAgenda.add(childID);
            }
        }
        return tree;
    }
    
    public static KahinaMemTree backwardExpansionFromNode(KahinaDAG dag, int startID)
    {
        KahinaMemTree tree = new KahinaMemTree();
        Map<Integer,Integer> treeToTagID = new TreeMap<Integer,Integer>();
        int rootID = tree.addNode(dag.getNodeCaption(startID), "", dag.getNodeStatus(startID));
        treeToTagID.put(rootID, startID);
        tree.setRootID(rootID);
        List<Integer> expansionAgenda = new LinkedList<Integer>();
        expansionAgenda.add(rootID);
        while (expansionAgenda.size() > 0)
        {
            int currentID = expansionAgenda.remove(0);
            for (int edgeID : dag.getIncomingEdges(treeToTagID.get(currentID)))
            {
                int endID = dag.getStartNode(edgeID);
                int childID = tree.addNode(dag.getNodeCaption(endID), "", dag.getNodeStatus(endID));
                treeToTagID.put(childID, endID);
                tree.addChild(currentID, childID);
                expansionAgenda.add(childID);
            }
        }
        return tree;
    }
}
