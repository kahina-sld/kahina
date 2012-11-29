package org.kahina.logic.sat.data.proof;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.core.data.tree.KahinaUnlayeredMemTree;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class ResolutionProofTree extends KahinaUnlayeredMemTree
{
    CnfSatInstance satInstance;
    Map<Integer,List<Integer>> clauses;
    
    public ResolutionProofTree(CnfSatInstance satInstance)
    {
        this.satInstance = satInstance;
        this.clauses = new TreeMap<Integer,List<Integer>>();
    }
    
    public List<Integer> getVarRelevanceOrdering()
    {
        List<Integer> ordering = new ArrayList<Integer>();
        Map<Integer,Integer> varCounts = new HashMap<Integer,Integer>();
        for (int clauseID : clauses.keySet())
        {
            System.err.println("clause " + clauseID + ": " + clauses.get(clauseID));
            for (int var : clauses.get(clauseID))
            {
                if (var < 0) var = -var;
                Integer previousCount = varCounts.get(var);
                if (previousCount == null)
                {
                    varCounts.put(var, 1);
                    ordering.add(var);
                }
                else
                {
                    varCounts.put(var, previousCount + 1);
                }
            }
        }
        Collections.sort(ordering, new VarRelevanceComparator(varCounts));
        return ordering;
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
    
    private static class VarRelevanceComparator implements Comparator<Integer>
    {
        Map<Integer,Integer> varCounts;
        
        public VarRelevanceComparator(Map<Integer,Integer> varCounts)
        {
            this.varCounts = varCounts;
        }
        
        @Override
        public int compare(Integer arg0, Integer arg1)
        {
            if (varCounts.get(arg0) == varCounts.get(arg1)) return 0;
            else if (varCounts.get(arg0) < varCounts.get(arg1)) return 1;
            else if (varCounts.get(arg0) > varCounts.get(arg1)) return -1;
            return 0;
        }    
    }
}
