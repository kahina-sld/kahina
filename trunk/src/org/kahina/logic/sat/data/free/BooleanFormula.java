package org.kahina.logic.sat.data.free;

import org.kahina.logic.sat.io.free.BooleanFormulaVisitor;

public abstract class BooleanFormula
{
    //used to represent pruned formula trees for selective Tseitin transformation
    private boolean pruned = false;
    
    public boolean isPruned()
    {
        return pruned;
    }
    
    public void setPruned(boolean pruned)
    {
        this.pruned = pruned;
    }
    
    public abstract String toStringWithMinimumBracing();

    public abstract int getSize();
    
    public abstract <A> A accept(BooleanFormulaVisitor<A> visitor);
}
