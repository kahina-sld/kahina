package org.kahina.logic.sat.data.free;

public class Negation extends BooleanFormula
{
    BooleanFormula fm;
    
    public Negation(BooleanFormula fm)
    {
        this.fm = fm;
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
}
