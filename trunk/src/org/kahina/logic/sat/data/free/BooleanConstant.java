package org.kahina.logic.sat.data.free;

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
}
