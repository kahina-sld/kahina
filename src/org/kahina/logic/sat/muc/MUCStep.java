package org.kahina.logic.sat.muc;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.core.KahinaStep;

public class MUCStep extends KahinaStep
{
    //IC = interesting constraint
    
    //state data (should be kept as a sorted structure to allow quick identity check!)
    private List<Integer> uc; //current unsatisfiable core (not necessarily minimal)
    
    //for each IC, store the state that we arrive in after removing it
    //null = unchecked (the links has not yet been established
    //-1 = we already know that removing the IC leads to a satisfiable clause set
    //otherwise: result is the ID of the step we end up in after removing the IC
    private Map<Integer,Integer> icRemovalLink; 
    
    //makes it possible to annotate UCs with satisfiability information
    boolean satisfiable;
    
    public MUCStep()
    {
        uc =  new ArrayList<Integer>();
        icRemovalLink = new TreeMap<Integer,Integer>();
        satisfiable = false;
    }

    public List<Integer> getUc()
    {
        return uc;
    }

    public Integer getIcStatus(int index)
    {
        Integer status = icRemovalLink.get(index);
        if (status == null) return 0;
        else if (status == -1) return 2;
        return 1;
    }
    
    //returns null if we have no information yet!
    public Integer getRemovalLink(int index)
    {
        return this.icRemovalLink.get(index);
    }
    
    public void setRemovalLink(int index, int link)
    {
        this.icRemovalLink.put(index,link);
    }
    
    //bit of a suboptimal hash function, but very easy to "compute"
    public int hashCode()
    {
        return uc.size();
    }
    
    public boolean equals(Object o)
    {
        if (o instanceof MUCStep)
        {
            List<Integer> otherUC = ((MUCStep) o).getUc();
            if (otherUC.size() == uc.size())
            {
                //insanely expensive due to not very helpful data structure (TODO: hashing or similar)
                for (int ic : otherUC)
                {
                    if (!uc.contains(ic)) return false;
                }
                return true;
            }
        }
        return false;
    }
    
    public boolean isSatisfiable()
    {
        return satisfiable;
    }
    
    public void setSatisfiable(boolean satisfiable)
    {
        this.satisfiable = satisfiable;
    }
    
    /**
     * Computes the relation of a given block of meta variables to the current UC.
     * @param block a list of meta variables (typically negative literals)
     * @return 1 if block is guaranteed to be in MUC (all red -> red, selectable), 
     *         2 if block was successfully removed from UC (all green -> green, selectable), 
     *         3 if no element of the block was contained in UC (-> grey), 
     *         0 otherwise (-> block, selectable)
     */
    public int relationToBlock(List<Integer> block)
    {
        int numRed = 0;
        int numGreen = 0;
        int numOut = 0;
        //in the block definitions, literals are negative
        for (int negLit : block)
        {
            if (!uc.contains(-negLit))
            {
                numOut++;
            }
            else
            {
                int icStatus = getIcStatus(-negLit);
                if (icStatus == 2)
                {
                    numRed++;
                }
                else if (icStatus == 1)
                {
                    numGreen++;
                }
            }
        }
        if (numGreen == block.size())
        {
            return 2;
        }
        if (numOut == block.size())
        {
            return 3;
        }
        if (numRed > 0)
        {
            return 1;
        }
        return 0;
    }
}
