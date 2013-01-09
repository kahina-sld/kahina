package org.kahina.logic.sat.muc.data;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class PartitionBlockHandler extends LiteralBlockHandler
{
    static final boolean VERBOSE = true;
    
    //main parameter, defines minimum block size
    static final int MIN_BLOCK_SIZE = 2;
    
    private int nextBlockID;
    
    //blocks are coded as list of integers for now, indexed by IDs
    Map<Integer,TreeSet<Integer>> blockList;
    
    //map from block IDs into clauses which use that block
    Map<Integer,List<List<Integer>>> blockClauses;
    //map from block IDs into the defining clauses (used for splitting)
    Map<Integer,Integer> blockDefClauses;
    //map from block IDs into the defining variables (used for splitting)
    Map<Integer,Integer> blockDefVar;
    //map from block-defining variables into block IDs (used for splitting)
    Map<Integer,Integer> blockVarBlockID;
    
    //reverse index of literals into blocks (defines a partition here)
    Map<Integer,Integer> blockIndex;
    
    //TODO: lookup structure to find duplicate blocks
    
    public PartitionBlockHandler(CnfSatInstance satInstance)
    {
        super(satInstance);
        blockList = new TreeMap<Integer,TreeSet<Integer>>();
        blockClauses = new TreeMap<Integer,List<List<Integer>>>();
        blockDefClauses = new TreeMap<Integer,Integer>();
        blockDefVar = new TreeMap<Integer,Integer>();
        blockVarBlockID = new TreeMap<Integer,Integer>();
        blockIndex = new TreeMap<Integer,Integer>();
        nextBlockID = 1;
    }
    
    public List<Integer> buildRepresentation(TreeSet<Integer> clause)
    {
        if (VERBOSE) System.err.println("buildRepresentation(size = " + clause.size() + ")");
        List<Integer> blockClause = new LinkedList<Integer>();
        int overlapIndex = findHighestOverlapBlock(clause);
        if (VERBOSE) System.err.println("  maxOverlapIndex: " + overlapIndex);
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
            if (VERBOSE) System.err.println("  maxOverlapBlock: size = " + blockList.get(overlapIndex).size());
            Overlap overlap = new Overlap(clause, blockList.get(overlapIndex));
            if (VERBOSE) System.err.println("  Overlap: (" + overlap.aMinusB.size() + "," + overlap.aIntersectB.size() + "," + overlap.bMinusA.size() + ")");
            if (overlap.aIntersectB.size() == clause.size())
            {
                blockClause.add(blockDefVar.get(overlapIndex));
                addBlockClausesEntry(overlapIndex,blockClause);
            }
            else
            {
                //separate representations for the two parts
                blockClause.addAll(buildRepresentation(overlap.aIntersectB));
                blockClause.addAll(buildRepresentation(overlap.aMinusB));
            }
        }
        if (VERBOSE) System.err.println("= " + blockClause + "");
        return blockClause;
    }
    
    public void ensureRepresentability(TreeSet<Integer> block)
    {
        if (VERBOSE) System.err.println("ensureRepresentability(" + block + ")");
        int overlapIndex = findHighestOverlapBlock(block);
        if (VERBOSE) System.err.println("  maxOverlapIndex: " + overlapIndex);
        if (VERBOSE) System.err.println("  maxOverlapBlock: " + blockList.get(overlapIndex));
        if (overlapIndex == -1)
        {
            if (block.size() >= 1)
            {
                defineNewBlock(block);
            }
        }
        else
        {
            Overlap overlap = new Overlap(block, blockList.get(overlapIndex));  
            if (VERBOSE) System.err.println("  Overlap: (" + overlap.aMinusB.size() + "," + overlap.aIntersectB.size() + "," + overlap.bMinusA.size() + ")");
            if (overlap.aIntersectB.size() >= 1)
            {
                splitBlock(overlapIndex, overlap.aIntersectB, overlap.bMinusA);
            }
            if (overlap.aMinusB.size() >= 1)
            {
                //recursive case for the rest
                ensureRepresentability(overlap.aMinusB);
            }     
        }
    }
    
    //splits the block with blockID, returning the blocks' new representation
    //the two arguments need to define a partition of the block with blockID
    private List<Integer> splitBlock(int blockID, TreeSet<Integer> block1, TreeSet<Integer> block2)
    {
       if (VERBOSE) System.err.println("splitBlock(" + blockID + "," + block1 + "," + block2 + ")");
       List<Integer> newRepresentation = new LinkedList<Integer>();
       if (block1.size() > 0 && block2.size() > 0)
       {
           int block1ID = defineNewBlock(block1);
           newRepresentation.add(blockDefVar.get(block1ID));
           int block2ID = defineNewBlock(block2);
           newRepresentation.add(blockDefVar.get(block2ID));
           blockReplacement(blockID, newRepresentation);
       }
       else
       {
           //one of the blocks is identical to the old block
           newRepresentation.add(blockDefVar.get(blockID));
       }
       return newRepresentation;
    }
    
    private void blockReplacement(int blockID, List<Integer> newRepresentation)
    {
        if (VERBOSE) System.err.println("  blockReplacement(" + blockID + "," + newRepresentation + ")");
        //determine the block IDs in the new block
        List<Integer> newBlockIDs = new LinkedList<Integer>();
        for (int literal : newRepresentation)
        {
            Integer newBlockID = blockVarBlockID.get(literal);
            if (newBlockID != null) newBlockIDs.add(newBlockID);
        } 
        
        //replace all occurrences of the old block, update blockClauses to reflect new usage
        int blockVar = blockDefVar.get(blockID);
        if (VERBOSE) System.err.println("    obsolete blockVar: " + blockVar);
        for (List<Integer> clause : getBlockClauses(blockID))
        {
            if (VERBOSE) System.err.println("    Replacement in clause: " + clause);
            clause.remove(new Integer(blockVar));
            clause.addAll(newRepresentation);
            if (VERBOSE) System.err.println("    Replacement completed: " + clause);
            for (int newBlockID : newBlockIDs)
            {
                addBlockClausesEntry(newBlockID, clause);
            }
        }
        
        //remove the defining clause for the block
        if (VERBOSE) System.err.println("    Removing clauseID: " + blockDefClauses.get(blockID));
        satInstance.removeClauseID(blockDefClauses.get(blockID)); 
        satInstance.announceChangedClauses();
        
        //remove entries for the replaced block from all the tables
        blockList.remove(blockID);
        blockClauses.remove(blockID);
        blockDefClauses.remove(blockID);
        blockDefVar.remove(blockID);
        blockVarBlockID.remove(blockVar);
    }
    
    public int defineNewBlock(TreeSet<Integer> block)
    {
        int blockID = nextBlockID++;
        if (VERBOSE) System.err.println("  new block " + blockID + ": " + block);
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
        int blockVar = satInstance.getHighestVar() + 1;
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
        int clauseID = satInstance.addClause(blockDefClause);
        satInstance.announceAddedClauses();
        blockDefClauses.put(blockID, clauseID);
        if (VERBOSE) System.err.println("  new block clause " + clauseID + ": " + blockDefClause);
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
    
    private List<List<Integer>> getBlockClauses(int blockID)
    {
        List<List<Integer>> clausesForBlock = blockClauses.get(blockID);
        if (clausesForBlock == null)
        {
            clausesForBlock = new LinkedList<List<Integer>>();
            blockClauses.put(blockID, clausesForBlock);
        }
        return clausesForBlock;
    }
    
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
    
    public int findHighestOverlapBlock(TreeSet<Integer> block)
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
    
    public Collection<TreeSet<Integer>> getBlocks()
    {
        return blockList.values();
    }

    //TODO: develop a more efficient way of maintaining and retrieving this list
    public List<TreeSet<Integer>> retrieveBlocks()
    {
        List<TreeSet<Integer>> blocks = new LinkedList<TreeSet<Integer>>();
        //the keys are enumerated in ascending order -> canonical order!
        for (int blockID : blockList.keySet())
        {
            blocks.add(blockList.get(blockID));
        }
        return blocks;
    }

    @Override
    public TreeSet<Integer> getBlock(int blockID)
    {
        return blockList.get(blockID);
    }
}
