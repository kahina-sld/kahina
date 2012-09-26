package org.kahina.logic.sat.data.proof;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class ResolutionProofDAGtoTreeConversion
{
    public static void fillTreeWithForwardExpansionFromNode(ResolutionProofDAG dag, int startID, ResolutionProofTree tree)
    {
        Map<Integer,Integer> treeToTagID = new TreeMap<Integer,Integer>();
        int rootID = tree.addNode(dag.getNodeCaption(startID), "", dag.getNodeStatus(startID));
        treeToTagID.put(rootID, startID);
        tree.setRootID(rootID);
        List<Integer> clause = dag.getClauseForNode(startID);
        if (clause != null) tree.setNodeClause(rootID, clause);
        List<Integer> expansionAgenda = new LinkedList<Integer>();
        expansionAgenda.add(rootID);
        while (expansionAgenda.size() > 0)
        {
            int currentID = expansionAgenda.remove(0);
            for (int edgeID : dag.getOutgoingEdges(treeToTagID.get(currentID)))
            {
                int endID = dag.getEndNode(edgeID);
                int childID = tree.addNode(dag.getNodeCaption(endID), "", dag.getNodeStatus(endID));
                clause = dag.getClauseForNode(endID);
                if (clause != null) tree.setNodeClause(childID, clause);
                treeToTagID.put(childID, endID);
                tree.addChild(currentID, childID);
                expansionAgenda.add(childID);
            }
        }    
    }
    
    public static void fillTreeWithBackwardExpansionFromNode(ResolutionProofDAG dag, int startID, ResolutionProofTree tree)
    {
        Map<Integer,Integer> treeToDagID = new TreeMap<Integer,Integer>();
        Set<Integer> inspectedDagID = new HashSet<Integer>();
        int rootID = tree.addNode(dag.getNodeCaption(startID), "", dag.getNodeStatus(startID));
        treeToDagID.put(rootID, startID);
        tree.setRootID(rootID);
        List<Integer> clause = dag.getClauseForNode(startID);
        if (clause != null) tree.setNodeClause(rootID, clause);
        List<Integer> expansionAgenda = new LinkedList<Integer>();
        expansionAgenda.add(rootID);
        while (expansionAgenda.size() > 0)
        {
            int currentID = expansionAgenda.remove(0);
            //System.err.print(currentID + ": ");
            for (int edgeID : dag.getIncomingEdges(treeToDagID.get(currentID)))
            {
                int endID = dag.getStartNode(edgeID);
                int childID = tree.addNode(dag.getNodeCaption(endID), "", dag.getNodeStatus(endID));
                //System.err.print(endID + "->" + childID + " ");
                if (!inspectedDagID.contains(endID)) expansionAgenda.add(childID);
                clause = dag.getClauseForNode(endID);
                if (clause != null) 
                {
                    tree.setNodeClause(childID, clause);
                }
                treeToDagID.put(childID, endID);
                tree.addChild(currentID, childID);
            }
            inspectedDagID.add(currentID);
            //System.err.println();
        }    
    }
}
