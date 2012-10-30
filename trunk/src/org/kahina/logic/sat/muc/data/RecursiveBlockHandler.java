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
    
    //top block for searching insertion and split points
    int topBlock;

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

    //TODO: decide whether this should be cashed (explicit tree structure?)
    private List<Integer> getSubblocks(int blockID)
    {
        List<Integer> subblocks = new LinkedList<Integer>();
        for (int literal : blockList.get(blockID))
        {
            Integer litBlockID = blockVarBlockID.get(literal);
            if (litBlockID != null)
            {
                subblocks.add(litBlockID);
            }
        }
        return subblocks;
    }

    @Override
    public List<Integer> buildRepresentation(List<Integer> clause)
    {
        List<Integer> representation = new LinkedList<Integer>();
        //the first block is always the top block
        if (blockList.size() == 0)
        {
            topBlock = defineNewBlock(clause);
            representation.add(blockDefVar.get(topBlock));
        }
        else
        {
            //TODO: special treatment for clause elements outside topBlock
            representation.addAll(buildRepresentation(clause, topBlock));
        }
        return representation;
    }
    
    private List<Integer> buildRepresentation(List<Integer> block, int blockID)
    {
        List<Integer> representation = new LinkedList<Integer>();
        Overlap overlap = new Overlap(block, blockList.get(blockID));
        //IDEA: only express those elements which are inside the reference block
        //TODO: there must be treatment for non-blocks (no strict nesting enforced?)  
        //just ignore overlap.aMinusB.size() here

        if (overlap.bMinusA.size() > 0)
        {
            List<Integer> subblocks = getSubblocks(blockID);
            if (subblocks.size() == 0)
            {
                //base case: leaf in block tree; split it into bMinusA and aIntersectB
                //TODO: split
            }
            else
            {
                //recursive case: the representation makes use of subblocks  
                for (int subblockID : subblocks)
                {
                    representation.addAll(buildRepresentation(block,subblockID));
                }
            }
        }
        else
        {
            if (overlap.aIntersectB.size() == 0)
            {
                //no overlap between the blocks => empty representation
            }
            else
            {
                //the representation is identical to the reference block
                representation.add(blockDefVar.get(blockID));
            }
        }
        return representation;
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
    
    public int defineNewBlock(List<Integer> block)
    {
        int blockID = satInstance.getNumClauses();
        blockList.put(blockID, block);
        satInstance.setNumClauses(blockID + 1);
        int blockVar = satInstance.getNumVars() + 1;
        satInstance.setNumVars(blockVar);
        blockDefVar.put(blockID, blockVar);
        blockVarBlockID.put(blockVar, blockID);
        List<Integer> blockDefClause = new LinkedList<Integer>();
        blockDefClause.add(-blockVar);
        //perhaps we don't even need the block index
        /*for (int literal : block)
        { 
            blockDefClause.add(literal);
            //update the reverse index (let literals point to new block)
            blockIndex.put(literal, blockID);
        }*/
        blockDefClauses.put(blockID, blockDefClause);
        satInstance.getClauses().add(blockDefClause);
        if (VERBOSE) System.err.println("  new block clause:" + blockDefClause);
        return blockID;
    }
}