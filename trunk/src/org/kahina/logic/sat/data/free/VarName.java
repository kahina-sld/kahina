package org.kahina.logic.sat.data.free;

public class VarName
{
    public final int num;
    
    private static int nextName = 1;

    public VarName() 
    {
        this.num = freshName();
    }
    
    public VarName(int n) 
    {
        this.num = n;
        if (num >= nextName)
        {
            nextName = n + 1;
        }
    }

    @Override
    public String toString() 
    {
        return "L" + num;
    }
    
    public static int freshName()
    {
        return nextName++;
    }
    
    public static void resetNames() 
    {
        nextName = 1;
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
      final VarName other = (VarName) obj;
      if (this.num != other.num) 
      {
        return false;
      }
      return true;
    }

    @Override
    public int hashCode() 
    {
      int hash = 7;
      hash = 79 * hash + this.num;
      return hash;
    }
}
