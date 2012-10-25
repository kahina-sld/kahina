package org.kahina.logic.sat.muc.data;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class MUCMetaInstance extends CnfSatInstance
{
    //TODO: consider recursive block hierarchies instead of partitions 
    //  + hierarchy of blocks allows for even shorter descriptions
    //  - expensive maintenance!
    
    //
    static final boolean VERBOSE = true;
    
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
    
    //reverse index of literals into blocks (defines a partition for now)
    Map<Integer,Integer> blockIndex;
    
    //TODO: lookup structure to find duplicate blocks
    
    public MUCMetaInstance(int numOrigClauses)
    {
        super();
        setNumVars(numOrigClauses);
        blockList = new TreeMap<Integer,List<Integer>>();
        blockClauses = new TreeMap<Integer,List<List<Integer>>>();
        blockDefClauses = new TreeMap<Integer,List<Integer>>();
        blockDefVar = new TreeMap<Integer,Integer>();
        blockVarBlockID = new TreeMap<Integer,Integer>();
        blockIndex = new TreeMap<Integer,Integer>();
        //blockIndex = new TreeMap<Integer,List<Integer>>();
    }
    
    public void learnNewClause(List<Integer> clause)
    {
        clauses.add(buildRepresentation(clause));
    }
    
    private List<Integer> buildRepresentation(List<Integer> clause)
    {
        if (VERBOSE) System.err.println("buildRepresentation(" + clause + ")");
        List<Integer> blockClause = new LinkedList<Integer>();
        int overlapIndex = findHighestOverlapBlock(clause);
        if (VERBOSE) System.err.println("  maxOverlapIndex: " + overlapIndex);
        if (VERBOSE) System.err.println("  maxOverlapBlock: " + blockList.get(overlapIndex));
        if (overlapIndex == -1)
        {
            if (clause.size() >= MIN_BLOCK_SIZE)
            {
                int blockID = defineNewBlock(clause);
                blockClause.add(blockDefVar.get(blockID));
                addBlockClausesEntry(blockID,blockClause);
            }
            else
            {
                blockClause.addAll(clause);
            }
        }
        else
        {
            Overlap overlap = new Overlap(clause, blockList.get(overlapIndex));
            List<Integer> bReplacement = splitBlock(overlapIndex, overlap.aIntersectB, overlap.bMinusA);
            if (overlap.aIntersectB.size() >= MIN_BLOCK_SIZE)
            {
                int intersectBlockVar = bReplacement.get(0);
                blockClause.add(intersectBlockVar);
            }
            else
            {
                blockClause.addAll(overlap.aIntersectB);
            }
            if (overlap.aMinusB.size() >= MIN_BLOCK_SIZE)
            {
                //recursive case for the rest
                blockClause.addAll(buildRepresentation(overlap.aMinusB));
            }     
            else
            {
                blockClause.addAll(overlap.aMinusB);
            }
        }
        if (VERBOSE) System.err.println("= " + blockClause + "");
        return blockClause;
    }
    
    //splits the block with blockID, returning the blocks' new representation
    //the two arguments need to define a partition of the block with blockID
    private List<Integer> splitBlock(int blockID, List<Integer> block1, List<Integer> block2)
    {
       List<Integer> newRepresentation = new LinkedList<Integer>();
       if (block2.size() > 0)
       {
           if (block1.size() >= MIN_BLOCK_SIZE)
           {
               int block1ID = defineNewBlock(block1);
               newRepresentation.add(blockDefVar.get(block1ID));
           }
           else
           {
               //these literals are without an assigned block now
               for (int block1Lit : block1)
               {
                   blockIndex.remove(block1Lit);
               }
               newRepresentation.addAll(block1);
           }
           if (block2.size() >= MIN_BLOCK_SIZE)
           {
               int block2ID = defineNewBlock(block2);
               newRepresentation.add(blockDefVar.get(block2ID));
           }
           else
           {
               //these literals are without an assigned block now
               for (int block2Lit : block2)
               {
                   blockIndex.remove(block2Lit);
               }
               newRepresentation.addAll(block2);
           }
           blockReplacement(blockID, newRepresentation);
       }
       else
       {
           //block1 is identical to the old block
           newRepresentation.add(blockDefVar.get(blockID));
       }
       return newRepresentation;
    }
    
    private void blockReplacement(int blockID, List<Integer> newRepresentation)
    {
        if (VERBOSE) System.err.println("blockReplacement(" + blockID + "," + newRepresentation + ")");
        //determine the block IDs in the new block
        List<Integer> newBlockIDs = new LinkedList<Integer>();
        for (int literal : newRepresentation)
        {
            Integer newBlockID = blockVarBlockID.get(literal);
            if (newBlockID != null) newBlockIDs.add(newBlockID);
        } 
        
        //replace all occurrences of the old block, update blockClauses to reflect new usage
        int blockVar = blockDefVar.get(blockID);
        for (List<Integer> clause : blockClauses.get(blockID))
        {
            clause.remove(new Integer(blockVar));
            clause.addAll(newRepresentation);
            for (int newBlockID : newBlockIDs)
            {
                addBlockClausesEntry(newBlockID, clause);
            }
        }
        
        //remove the defining clause for the block
        clauses.remove(blockDefClauses.get(blockID)); 
        
        //remove entries for the replaced block from all the tables
        blockList.remove(blockID);
        blockClauses.remove(blockID);
        blockDefClauses.remove(blockID);
        blockDefVar.remove(blockID);
        blockVarBlockID.remove(blockVar);
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
        int blockVar = ++numVars;
        blockDefVar.put(blockID, blockVar);
        blockVarBlockID.put(blockVar, blockID);
        List<Integer> blockDefClause = new LinkedList<Integer>();
        blockDefClause.add(-blockVar);
        for (int literal : block)
        { 
            blockDefClause.add(literal);
            //update the reverse index (let literals point to new block)
            blockIndex.put(literal, blockID);
        }
        blockDefClauses.put(blockID, blockDefClause);
        clauses.add(blockDefClause);
        if (VERBOSE) System.err.println("  new block clause:" + blockDefClause);
        return blockID;
    }
    
    /*private void addReverseIndexItem(int literal, int blockID)
    {
        List<Integer> blocksForLiteral = blockIndex.get(literal);
        if (blocksForLiteral == null)
        {
            blocksForLiteral = new LinkedList<Integer>();
            blockIndex.put(literal, blocksForLiteral);
        }
        blocksForLiteral.add(blockID);
    }*/
    
    private void addBlockClausesEntry(int blockID, List<Integer> clause)
    {
        List<List<Integer>> clausesForBlock = blockClauses.get(blockID);
        if (clausesForBlock == null)
        {
            clausesForBlock = new LinkedList<List<Integer>>();
            blockClauses.put(blockID, clausesForBlock);
        }
        clausesForBlock.add(clause);
    }
    
    public int findHighestOverlapBlock(List<Integer> block)
    {
        //use reverse index to count overlaps
        Map<Integer,Integer> overlapCount = new TreeMap<Integer,Integer>();
        for (int literal : block)
        {
            Integer blockForLit = blockIndex.get(literal);
            if (blockForLit != null)
            {
                increaseCount(overlapCount, blockForLit);
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
}
