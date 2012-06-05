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
}
