package org.kahina.logic.sat.muc.data;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class BlocklessBlockHandler extends LiteralBlockHandler
{
    public BlocklessBlockHandler(CnfSatInstance satInstance)
    {
        super(satInstance);
        // TODO Auto-generated constructor stub
    }

    @Override
    public List<Integer> buildRepresentation(TreeSet<Integer> clause)
    {
        List<Integer> representation = new LinkedList<Integer>();
        representation.addAll(clause);
        return representation;
    }

    @Override
    public void ensureRepresentability(TreeSet<Integer> block)
    {
        //do nothing, we do not register blocks
    }

    @Override
    public TreeSet<Integer> getBlock(int blockID)
    {
        //we do not support any blocks
        return null;
    }

    @Override
    public Collection<TreeSet<Integer>> getBlocks()
    {
        //we do not support any blocks, so we return the empty set
        return new LinkedList<TreeSet<Integer>>();
    }

}
