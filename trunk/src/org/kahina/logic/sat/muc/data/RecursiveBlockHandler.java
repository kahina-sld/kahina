package org.kahina.logic.sat.muc.data;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class RecursiveBlockHandler extends LiteralBlockHandler
{
    //recursive block hierarchy instead of partitions 
    //  + hierarchy of blocks allows for even shorter descriptions
    //  + possibly very interesting structure for interactive MUS extraction
    //  - expensive maintenance!
    
    static final boolean VERBOSE = false;
    
    //main parameter, defines minimum block size
    static final int MIN_BLOCK_SIZE = 1;
    
    //blocks are coded as tree sets of integers for now, indexed by IDs
    Map<Integer,TreeSet<Integer>> blockList;
    
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
    
    private boolean needsUpdate = true;

    public RecursiveBlockHandler(CnfSatInstance satInstance)
    {
        super(satInstance);
        blockList = new TreeMap<Integer,TreeSet<Integer>>();
        blockClauses = new TreeMap<Integer,List<List<Integer>>>();
        blockDefClauses = new TreeMap<Integer,List<Integer>>();
        blockDefVar = new TreeMap<Integer,Integer>();
        blockVarBlockID = new TreeMap<Integer,Integer>();
        blockIndex = new TreeMap<Integer,List<Integer>>();
        
        TreeSet<Integer> topBlockSet = new TreeSet<Integer>();
        //the top block should include all the meta variables in the beginning
        for (int i = 1; i <= satInstance.getNumVars(); i++)
        {
            topBlockSet.add(-i);
        }
        topBlock = defineNewBlock(topBlockSet);
    }

    //TODO: decide whether this should be cashed (explicit tree structure?)
    public List<Integer> getSubblocks(int blockID)
    {
        //System.err.println("getSubblocks(" + blockID + ")");
        List<Integer> subblocks = new LinkedList<Integer>();
        //System.err.println("  block = " + blockList.get(blockID));
        for (int literal : blockDefClauses.get(blockID))
        {
            Integer litBlockID = blockVarBlockID.get(literal);
            if (litBlockID != null)
            {
                subblocks.add(litBlockID);
            }
        }
        //System.err.println("  subblocks = " + subblocks);
        return subblocks;
    }
    
    @Override
    public List<Integer> buildRepresentation(TreeSet<Integer> clause)
    {
        List<Integer> representation = new LinkedList<Integer>();
        representation.addAll(buildRepresentation(clause, topBlock));
        return representation;
    }
    
    private List<Integer> buildRepresentation(TreeSet<Integer> block, int blockID)
    {
        List<Integer> representation = new LinkedList<Integer>();
        Overlap overlap = new Overlap(block, blockList.get(blockID));
        /*System.err.println("Overlap(block.size() = " + block.size() + ", blockID = " + blockID + "):" );
        System.err.println("  aIntersectB.size() = " + overlap.aIntersectB.size());
        System.err.println("  aMinusB.size()     = " + overlap.aMinusB.size());
        System.err.println("  bMinusA.size()     = " + overlap.bMinusA.size());*/
        //IDEA: only express those elements which are inside the reference block
        //just ignore overlap.aMinusB.size() here, this is handled by other calls
        if (overlap.bMinusA.size() > 0)
        {
            List<Integer> subblocks = getSubblocks(blockID);
            if (subblocks.size() == 0)
            {
                representation.addAll(overlap.aIntersectB);
            }
            else
            {
                //recursive case: the representation makes use of subblocks  
                //System.err.println("  Subblocks: " + subblocks);
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
                //no overlap between the blocks => no additional element in representation
            }
            else
            {
                //the representation is identical to the reference block
                representation.add(blockDefVar.get(blockID));
            }
        }
        //System.err.println("Derived Representation: " + representation);
        return representation;
    }
    
    /*private List<Integer> removeFromBlock(int blockID, List<Integer> removals)
    {
        //adapt the block definition to not include the removed elements any more
        
        //adapt the clauses which use the modified block
        for (List<Integer> clause : blockClauses.get(blockID))
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
        //recursion: we also need to remove things from the relevant subblocks
        
    }*/
    
    public void ensureRepresentability(TreeSet<Integer> block)
    {
        ensureRepresentability(block, topBlock);
        needsUpdate = true;
    }
    
    private void ensureRepresentability(TreeSet<Integer> block, int blockID)
    {
        Overlap overlap = new Overlap(block, blockList.get(blockID));
        /*System.err.println("Overlap(block.size() = " + block.size() + ", blockID = " + blockID + "):" );
        System.err.println("  aIntersectB.size() = " + overlap.aIntersectB.size());
        System.err.println("  aMinusB.size()     = " + overlap.aMinusB.size());
        System.err.println("  bMinusA.size()     = " + overlap.bMinusA.size());*/
        //IDEA: only express those elements which are inside the reference block
        //just ignore overlap.aMinusB.size() here, this is handled by other calls
        if (overlap.bMinusA.size() > 0)
        {
            List<Integer> subblocks = getSubblocks(blockID);
            if (subblocks.size() == 0)
            {
                if (overlap.aIntersectB.size() >= MIN_BLOCK_SIZE)
                {
                    splitBlock(blockID, overlap.aIntersectB, overlap.bMinusA);
                }
            }
            else
            {
                //recursive case: the representation makes use of subblocks  
                //System.err.println("  Subblocks: " + subblocks);
                for (int subblockID : subblocks)
                {
                    ensureRepresentability(block,subblockID);
                }
            }
        }
    }
    
    //splits the block with blockID, returning the blocks' new representation
    //the two arguments need to define a partition of the block with blockID
    private List<Integer> splitBlock(int blockID, TreeSet<Integer> block1, TreeSet<Integer> block2)
    {
       List<Integer> newRepresentation = new LinkedList<Integer>();
       if (block2.size() > 0 && block1.size() > 0)
       {
           //rebuild the defining clause for the block being split
           List<Integer> definingClause = blockDefClauses.get(blockID);
           definingClause.clear();
           definingClause.add(-blockDefVar.get(blockID));
           if (block1.size() >= MIN_BLOCK_SIZE)
           {
               int block1ID = defineNewBlock(block1);
               int block1Var = blockDefVar.get(block1ID);
               newRepresentation.add(block1Var);
               definingClause.add(block1Var);
           }
           else
           {
               newRepresentation.addAll(block1);
               definingClause.addAll(block1);
           }
           if (block2.size() >= MIN_BLOCK_SIZE)
           {
               int block2ID = defineNewBlock(block2);
               int block2Var = blockDefVar.get(block2ID);
               newRepresentation.add(block2Var);
               definingClause.add(block2Var);
           }
           else
           {
               newRepresentation.addAll(block2);
               definingClause.addAll(block2);
           }
           satInstance.announceChangedClauses();
       }
       else
       {
           //block1 is identical to the old block
           newRepresentation.add(blockDefVar.get(blockID));
       }
       return newRepresentation;
    }

    @Override
    public Collection<TreeSet<Integer>> getBlocks()
    {
        //TODO: this should actually return a tree of blocks!
        return blockList.values();
    }
    
    public int defineNewBlock(TreeSet<Integer> block)
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
        for (int literal : block)
        { 
            blockDefClause.add(literal);
            //update the reverse index (let literals point to new block)
            //blockIndex.put(literal, blockID);
        }
        blockDefClauses.put(blockID, blockDefClause);
        satInstance.getClauses().add(blockDefClause);
        satInstance.announceAddedClauses();
        needsUpdate = true;
        if (VERBOSE) System.err.println("  new block clause:" + blockDefClause);
        return blockID;
    }
    
    //TODO: develop a more efficient way of maintaining and retrieving this list
    public KahinaTree retrieveBlockTree()
    {
        KahinaTree tree = new KahinaMemTree();
        for (int blockID : blockList.keySet())
        {
            tree.addNode(blockID, blockDefVar.get(blockID) + " (" + blockList.get(blockID).size() + ")", "", 0);
        }
        tree.setRootID(topBlock);
        List<Integer> agenda = new LinkedList<Integer>();
        agenda.add(topBlock);
        while (!agenda.isEmpty())
        {
            int parent = agenda.remove(0);
            for (int child : getSubblocks(parent))
            {
                tree.addChild(parent, child);
                agenda.add(child);
            }
        }
        return tree;
    }
    
    @Override
    public TreeSet<Integer> getBlock(int blockID)
    {
        return blockList.get(blockID);
    }
    
    public void announceChange()
    {
        needsUpdate = true;
    }
    
    public boolean needsUpdate()
    {
        if (needsUpdate)
        {
            needsUpdate = false;
            return true;
        }
        else 
        {
            return false;
        }
    }
}
