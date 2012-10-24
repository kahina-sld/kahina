package org.kahina.logic.sat.muc.data;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class MUCMetaInstance extends CnfSatInstance
{
    //TODO: consider recursive block hierarchies (expensive maintenance!)
    
    //blocks are coded as list of integers for now
    Map<Integer,List<Integer>> blockList;
    
    //reverse index of literals into blocks
    Map<Integer,List<Integer>> blockIndex;
    
    //TODO: lookup structure to find duplicate blocks
    
    public MUCMetaInstance()
    {
        super();
        blockList = new TreeMap<Integer,List<Integer>>();
        blockIndex = new TreeMap<Integer,List<Integer>>();
    }
    
    public int defineNewBlock(List<Integer> block)
    {
        int blockID = numClauses;
        numClauses += block.size();
        blockList.put(blockID, block);
        for (int literal : block)
        {
            List<Integer> blockDefClause = new LinkedList<Integer>();
            blockDefClause.add(literal);
            blockDefClause.add(-blockID);
            addReverseIndexItem(literal, blockID);
            clauses.add(blockDefClause);
        }
        return blockID;
    }
    
    private void addReverseIndexItem(int literal, int blockID)
    {
        List<Integer> blocksForLiteral = blockIndex.get(literal);
        if (blocksForLiteral == null)
        {
            blocksForLiteral = new LinkedList<Integer>();
            blockIndex.put(literal, blocksForLiteral);
        }
        blocksForLiteral.add(blockID);
    }
    
    public int findHighestOverlapBlock(List<Integer> block)
    {
        //use reverse index to count overlaps
        Map<Integer,Integer> overlapCount = new TreeMap<Integer,Integer>();
        for (int literal : block)
        {
            List<Integer> blocksForLiteral = blockIndex.get(literal);
            if (blocksForLiteral != null)
            {
                for (int blockForLit : blocksForLiteral)
                {
                    increaseCount(overlapCount, blockForLit);
                }
            }
        }
        //search for maximum overlap
        int maxIndex = -1; //-1 <=> no overlap
        int maxOverlap = 0;
        for (int blockIndex : overlapCount.keySet())
        {
            Integer overlap = overlapCount.get(blockIndex);
            if (overlap != null)
            {
                if (overlap > maxOverlap)
                {
                    maxIndex = blockIndex;
                    maxOverlap = overlap;
                }
            }
        }
        return maxIndex;
    }
    
    private void increaseCount(Map<Integer,Integer> counters, int index)
    {
        Integer count = counters.get(index);
        if (count == null) count = 1;
        counters.put(index, count);
    }
}
