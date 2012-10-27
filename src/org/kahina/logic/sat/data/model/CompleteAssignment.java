package org.kahina.logic.sat.data.model;

import java.util.BitSet;
import java.util.List;

//for handling complete assignments, BitSets are used

public class CompleteAssignment
{
    BitSet table;
    
    public CompleteAssignment()
    {
        table = new BitSet();
    }
    
    public void setTrue(int var)
    {
        table.set(var);
    }
    
    public void setFalse(int var)
    {
        table.clear(var);
    }
    
    public void flipVar(int var)
    {
        table.flip(var);
    }
    
    public boolean getValue(int var)
    {
        return table.get(var);
    }
    
    public boolean satisfies(List<Integer> clause)
    {
        for (int lit : clause)
        {
            if (lit < 0)
            {
                if (!table.get(-lit)) return true;
            }
            if (lit > 0)
            {
                if (table.get(lit)) return true;
            }
        }
        return false;
    }
    
    public String toString()
    {
        StringBuilder str = new StringBuilder("{");
        for (int i = 1; i < table.length(); i++)
        {
            if (!table.get(i)) str.append("-");
            str.append(i + "");
            str.append(",");
        }
        str.deleteCharAt(str.length() - 1);
        str.append("}");
        return str.toString();
    }
}
