package org.kahina.logic.sat.muc;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

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
    private Map<Integer,Integer> reductionTable; 
    
    //makes it possible to annotate UCs with satisfiability information
    boolean satisfiable;
    
    public MUCStep()
    {
        uc =  new ArrayList<Integer>();
        reductionTable = new TreeMap<Integer,Integer>();
        satisfiable = false;
    }

    public List<Integer> getUc()
    {
        return uc;
    }
    
    public int getStepType()
    {
        int numberRed = 0;
        boolean hasLightGreen = false;
        for (int clauseID : uc)
        {
            Integer status = reductionTable.get(clauseID);
            if (status == null) return MUCStepType.UNKNOWN;
            if (status == -1) numberRed++;
            if (status == -2) hasLightGreen = true;
        }
        if (numberRed == uc.size())
        {
            //the status for a MUC
            return 2;
        }
        if (hasLightGreen)
        {
            return 3;
        }
        return 1;
    }

    public Integer getIcStatus(int index)
    {
        Integer status = reductionTable.get(index);
        if (status == null) return 0;
        else if (status == -1) return 2;
        else if (status == -2) return 3;
        return 1;
    }
    
    //returns null if we have no information yet!
    public Integer getRemovalLink(int index)
    {
        return this.reductionTable.get(index);
    }
    
    public void setRemovalLink(int index, int link)
    {
        this.reductionTable.put(index,link);
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
                for (int ic : otherUC)
                {
                    if (!uc.contains(ic)) return false;
                }
                return true;
            }
        }
        return false;
    }
    
    public int numUnknownClauses()
    {
        return uc.size() - reductionTable.size();
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
     *         4 if a part of the block is guaranteed to be in MUC (some red -> light red, selectable),
     *         5 if a part of the block is guaranteed to be in MUC (some green -> lght green, selectable),
     *         0 otherwise (-> block, selectable)
     */
    public int relationToBlock(TreeSet<Integer> block)
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
        if (numRed == block.size())
        {
            return 1;
        }
        if (numRed > 0)
        {
            return 4;
        }
        if (numGreen > 0)
        {
            return 5;
        }
        return 0;
    }
}
