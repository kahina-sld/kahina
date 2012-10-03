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
}
