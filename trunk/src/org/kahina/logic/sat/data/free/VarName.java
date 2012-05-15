package org.kahina.logic.sat.data.free;

public class VarName
{
    final int number;
    
    private static int nextName = 1;

    public VarName() 
    {
        this.number = freshName();
    }
    
    public VarName(int n) 
    {
        this.number = n;
    }

    @Override
    public String toString() 
    {
        return "L" + number;
    }
    
    public static int freshName()
    {
        return nextName++;
    }
    
    public static void resetNames() 
    {
        nextName = 1;
    }
}
