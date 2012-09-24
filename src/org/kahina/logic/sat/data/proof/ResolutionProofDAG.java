package org.kahina.logic.sat.data.proof;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.core.data.dag.KahinaMemDAG;
import org.kahina.logic.sat.data.KahinaSatInstance;

public class ResolutionProofDAG extends KahinaMemDAG
{
    KahinaSatInstance satInstance;
    Map<Integer,List<Integer>> clauses;
    
    public ResolutionProofDAG(KahinaSatInstance satInstance)
    {
        this.satInstance = satInstance;
        this.clauses = new TreeMap<Integer,List<Integer>>();
    }
    
    public void setNodeClause(int id, List<Integer> clause)
    {
        clauses.put(id, clause);
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
