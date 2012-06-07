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
    
    @Override
    public boolean equals(Object obj) 
    {
      if (obj == null) 
      {
        return false;
      }
      if (getClass() != obj.getClass()) 
      {
        return false;
      }
      final BooleanVariable other = (BooleanVariable) obj;
      if (this.var != other.var && (this.var == null || !this.var.equals(other.var))) 
      {
        return false;
      }
      return true;
    }

    @Override
    public int hashCode() 
    {
      int hash = 3;
      hash = 41 * hash + (this.var != null ? this.var.hashCode() : 0);
      return hash;
    }
}
