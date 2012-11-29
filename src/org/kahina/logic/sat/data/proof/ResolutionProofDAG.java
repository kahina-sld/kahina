package org.kahina.logic.sat.data.proof;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.core.data.dag.KahinaMemDAG;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class ResolutionProofDAG extends KahinaMemDAG
{
    CnfSatInstance satInstance;
    Map<Integer,List<Integer>> clauses;
    int refutationNode;
    
    public ResolutionProofDAG(CnfSatInstance satInstance)
    {
        this.satInstance = satInstance;
        this.clauses = new TreeMap<Integer,List<Integer>>();
        this.refutationNode = -1;
    }
    
    public int getRefutationNode()
    {
        //if an empty clause was detected, return its ID
        if (refutationNode != -1)
        {
            return refutationNode;
        }
        //otherwise, just return the highest ID 
        //(= the clause last mentioned in the proof)
        return nextNodeID - 1;
    }
    
    public void setNodeClause(int id, List<Integer> clause)
    {
        clauses.put(id, clause);
        //detect a refutation clause
        if (clause.size() == 0)
        {
            refutationNode = id;
        }
    }
    
    public List<Integer> getClauseForNode(int nodeID)
    {
        return clauses.get(nodeID);
    }
    
    public String getNodeCaption(int id)
    {
        List<Integer> clause = clauses.get(id);
        if (clause == null) return super.getNodeCaption(id);
        String nodeCaption = "";
        for (Integer lit : clause)
        {
           nodeCaption += satInstance.getSymbolForLiteral(lit) + " ";
        }
        return nodeCaption;
    }
}
