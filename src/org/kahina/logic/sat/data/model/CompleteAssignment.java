package org.kahina.logic.sat.data.model;

import java.util.BitSet;

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
