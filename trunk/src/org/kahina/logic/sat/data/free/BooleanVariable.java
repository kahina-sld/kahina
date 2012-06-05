package org.kahina.logic.sat.data.free;

import org.kahina.logic.sat.io.free.BooleanFormulaVisitor;

public class BooleanVariable extends BooleanFormula
{
    final VarName var;
    
    public BooleanVariable(VarName var) 
    {
        this.var = var;
    }
    
    public VarName getName()
    {
        return var;
    }
    
    @Override
    public String toString() 
    {
      return var.toString();
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
        return visitor.visitVariable(this);
    }
}
