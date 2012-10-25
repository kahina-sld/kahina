package org.kahina.logic.sat.muc.data;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class MUCMetaInstance extends CnfSatInstance
{
    //TODO: consider recursive block hierarchies (expensive maintenance!)
    
    //main parameter, defines minimum block size
    static final int MIN_BLOCK_SIZE = 3;
    
    //blocks are coded as list of integers for now, indexed by IDs
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
    
    public void learnNewClause(List<Integer> clause)
    {
        List<Integer> blockClause = new LinkedList<Integer>();
        int overlapIndex = findHighestOverlapBlock(clause);
        if (overlapIndex == -1)
        {
            int blockID = defineNewBlock(clause);
            blockClause.add(blockID);

        }
        else
        {
            Overlap overlap = new Overlap(clause, blockList.get(overlapIndex));
            if (overlap.aIntersectB.size() >= MIN_BLOCK_SIZE)
            {
                int aIntersectBid = defineNewBlock(overlap.aIntersectB);
                blockClause.add(aIntersectBid);
                //TODO: perform the block split
                if (overlap.bMinusA.size() >= MIN_BLOCK_SIZE)
                {
                    
                }
            }
            else
            {
                blockClause.addAll(overlap.aIntersectB);
            }
            if (overlap.aMinusB.size() >= MIN_BLOCK_SIZE)
            {
                int aMinusBid = defineNewBlock(overlap.aMinusB);
                blockClause.add(aMinusBid);
            }     
            else
            {
                blockClause.addAll(overlap.aMinusB);
            }
        }
        clauses.add(blockClause);
    }
    
    public int defineNewBlock(List<Integer> block)
    {
        int blockID = numClauses;
        blockList.put(blockID, block);
        //this side of the implication is not needed
        /*numClauses += block.size();
        for (int literal : block)
        {
            List<Integer> blockDefClause = new LinkedList<Integer>();
            blockDefClause.add(literal);
            blockDefClause.add(-blockID);
            addReverseIndexItem(literal, blockID);
            clauses.add(blockDefClause);
        }*/
        numClauses++;
        List<Integer> blockDefClause = new LinkedList<Integer>();
        for (int literal : block)
        { 
            blockDefClause.add(literal);
            addReverseIndexItem(literal, blockID);
        }
        blockDefClause.add(-blockID);
        clauses.add(blockDefClause);
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
    
    private class Overlap
    {
        public List<Integer> aIntersectB;
        public List<Integer> aMinusB;
        public List<Integer> bMinusA;
        
        public Overlap(List<Integer> a, List<Integer> b)
        {
            aIntersectB = new LinkedList<Integer>();
            aMinusB = new LinkedList<Integer>();
            bMinusA = new LinkedList<Integer>();
            for (Integer aEl : a)
            {
                if (b.contains(aEl))
                {
                    aIntersectB.add(aEl);
                }
                else
                {
                    aMinusB.add(aEl);
                }
            }
            for (Integer bEl : b)
            {
                if (!a.contains(bEl))
                {
                    bMinusA.add(bEl);
                }
            }
        }
    }
}
