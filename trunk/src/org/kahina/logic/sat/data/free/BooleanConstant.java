package org.kahina.logic.sat.data.free;

import org.kahina.logic.sat.io.free.BooleanFormulaVisitor;

public class BooleanConstant extends BooleanFormula
{
    boolean value;
    
    public BooleanConstant(boolean value)
    {
        this.value = value;
    }
    
    @Override
    public String toString() 
    {
        return value ? "T" : "F";
    }

    @Override
    public String toStringWithMinimumBracing()
    {
        return toString();
    }

    @Override
    public int getSize()
    {
        return 1;
    }
    
    public <A> A accept(BooleanFormulaVisitor<A> visitor) 
    {
        return visitor.visitConstant(this);
    }
}
