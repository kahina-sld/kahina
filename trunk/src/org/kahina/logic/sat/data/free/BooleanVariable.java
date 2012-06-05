package org.kahina.logic.sat.data.free;

public class BooleanVariable extends BooleanFormula
{
    final VarName var;
    
    public BooleanVariable(VarName var) 
    {
        this.var = var;
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
}
