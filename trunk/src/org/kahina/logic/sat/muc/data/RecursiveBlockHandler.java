package org.kahina.logic.sat.muc.data;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class RecursiveBlockHandler extends LiteralBlockHandler
{
    //recursive block hierarchy instead of partitions 
    //  + hierarchy of blocks allows for even shorter descriptions
    //  + possibly very interesting structure for interactive MUS extraction
    //  - expensive maintenance!
    
    static final boolean VERBOSE = false;
    
    //main parameter, defines minimum block size
    static final int MIN_BLOCK_SIZE = 3;
    
    //blocks are coded as list of integers for now, indexed by IDs
    Map<Integer,List<Integer>> blockList;
    
    //map from block IDs into clauses which use that block
    Map<Integer,List<List<Integer>>> blockClauses;
    //map from block IDs into the defining clauses (used for splitting)
    Map<Integer,List<Integer>> blockDefClauses;
    //map from block IDs into the defining variables (used for splitting)
    Map<Integer,Integer> blockDefVar;
    //map from block-defining variables into block IDs (used for splitting)
    Map<Integer,Integer> blockVarBlockID;
    
    //reverse index of literals into blocks where they occur
    Map<Integer,List<Integer>> blockIndex;

    public RecursiveBlockHandler(CnfSatInstance satInstance)
    {
        super(satInstance);
        blockList = new TreeMap<Integer,List<Integer>>();
        blockClauses = new TreeMap<Integer,List<List<Integer>>>();
        blockDefClauses = new TreeMap<Integer,List<Integer>>();
        blockDefVar = new TreeMap<Integer,Integer>();
        blockVarBlockID = new TreeMap<Integer,Integer>();
        blockIndex = new TreeMap<Integer,List<Integer>>();
    }

    @Override
    public List<Integer> buildRepresentation(List<Integer> clause)
    {
        //TODO like PartitionBlockHandler, but different implementation of splitting
        return null;
    }
    
    public int findHighestOverlapBlock(List<Integer> block)
    {
        //use reverse index to count overlaps
        Map<Integer,Integer> overlapCount = new TreeMap<Integer,Integer>();
        for (int literal : block)
        {
            List<Integer> blocksForLit = blockIndex.get(literal);
            if (blocksForLit != null)
            {
                for (int blockForLit : blocksForLit)
                {
                    increaseCount(overlapCount, blockForLit);
                }
            }
            /*List<Integer> blocksForLiteral = blockIndex.get(literal);
            if (blocksForLiteral != null)
            {
                for (int blockForLit : blocksForLiteral)
                {
                    increaseCount(overlapCount, blockForLit);
                }
            }*/
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

    @Override
    public Collection<List<Integer>> getBlocks()
    {
        //TODO: this should actually return a tree of blocks!
        return blockList.values();
    }
}
