package org.kahina.logic.sat.data.free;

import org.kahina.logic.sat.io.free.BooleanFormulaVisitor;

public class Negation extends BooleanFormula
{
    BooleanFormula fm;
    
    public Negation(BooleanFormula fm)
    {
        this.fm = fm;
    }
    
    public BooleanFormula getArg()
    {
        return fm;
    }
    
    @Override
    public String toString() 
    {
      return "-(" + fm + ")";
    }

    @Override
    public String toStringWithMinimumBracing()
    {
        if (fm instanceof BooleanVariable || fm instanceof BooleanConstant)
        {
            return "-" + fm.toStringWithMinimumBracing();
        }
        else
        {
            return "-(" + fm.toStringWithMinimumBracing() + ")";
        }
    }

    @Override
    public int getSize()
    {
        return fm.getSize();
    }
    
    public <A> A accept(BooleanFormulaVisitor<A> visitor) 
    {
        return visitor.visitNegation(this);
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
      final Negation other = (Negation) obj;
      if (this.fm != other.fm && (this.fm == null || !this.fm.equals(other.fm))) 
      {
        return false;
      }
      return true;
    }

    @Override
    public int hashCode() 
    {
      int hash = 5;
      hash = 37 * hash + (this.fm != null ? this.fm.hashCode() : 0);
      return hash;
    }
}
