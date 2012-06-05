package org.kahina.logic.sat.data.free;

import java.util.List;

import org.kahina.logic.sat.io.free.BooleanFormulaVisitor;

public class Disjunction extends BooleanFormula
{
    final List<BooleanFormula> fms;
    
    public Disjunction(List<BooleanFormula> fms) 
    {
        this.fms = fms;
    }
    
    public List<BooleanFormula> getDisjuncts()
    {
        return fms;
    }
    
    @Override
    public String toString() 
    {
      StringBuffer s = new StringBuffer();
      s.append("(");
      String separator = "";
      for(BooleanFormula fm : fms) 
      {
        s.append(separator);
        s.append(fm.toString());
        separator = "/";
      }
      s.append(")");
      return s.toString();
    }
    
    @Override
    public String toStringWithMinimumBracing()
    {
        StringBuffer s = new StringBuffer();
        String separator = "";
        for(BooleanFormula fm : fms) 
        {
          s.append(separator);
          if (fm instanceof Disjunction)
          {
              s.append("(" + fm.toStringWithMinimumBracing() + ")");
          }
          else
          {
              s.append(fm.toStringWithMinimumBracing());
          }
          separator = "/";
        }
        return s.toString();
    }
    
    public List<BooleanFormula> getFms()
    {
        return fms;
    }

    @Override
    public int getSize()
    {
        int size = 1;
        for (BooleanFormula fm : fms)
        {
            size += fm.getSize();
        }
        return size;
    }
    
    public <A> A accept(BooleanFormulaVisitor<A> visitor) 
    {
        return visitor.visitDisjunction(this);
    }
}
