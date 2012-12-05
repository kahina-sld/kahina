package org.kahina.logic.sat.muc.data;

import java.util.List;
import java.util.TreeSet;

public class Overlap
{
    public TreeSet<Integer> aIntersectB;
    public TreeSet<Integer> aMinusB;
    public TreeSet<Integer> bMinusA;
    
    public Overlap(TreeSet<Integer> a, TreeSet<Integer> b)
    {
        aIntersectB = new TreeSet<Integer>();
        aMinusB = new TreeSet<Integer>();
        bMinusA = new TreeSet<Integer>();
        for (Integer aEl : a)
        {
            if (b.contains(aEl))
            {
                aIntersectB.add(aEl);
            }
            else
            {
                aMinusB.add(aEl);
            }
        }
        for (Integer bEl : b)
        {
            if (!a.contains(bEl))
            {
                bMinusA.add(bEl);
            }
        }
    }
    
    /**
     * Computes the overlap of two lists, very expensive compared to the TreeSet variant!
     * @param a
     * @param b
     */
    public Overlap(List<Integer> a, List<Integer> b)
    {
        aIntersectB = new TreeSet<Integer>();
        aMinusB = new TreeSet<Integer>();
        bMinusA = new TreeSet<Integer>();
        for (Integer aEl : a)
        {
            if (b.contains(aEl))
            {
                aIntersectB.add(aEl);
            }
            else
            {
                aMinusB.add(aEl);
            }
        }
        for (Integer bEl : b)
        {
            if (!a.contains(bEl))
            {
                bMinusA.add(bEl);
            }
        }
    }
    
    public String toString()
    {
        StringBuilder s = new StringBuilder();
        s.append("aIntersectB " + aIntersectB.size() + ": {");
        for (int el : aIntersectB)
        {
            s.append(el + ",");
        }
        if (aIntersectB.size() > 0) s.deleteCharAt(s.length() - 1);
        s.append("}\n");
        s.append("aMinusB " + aMinusB.size() + ": {");
        for (int el : aMinusB)
        {
            s.append(el + ",");
        }
        if (aMinusB.size() > 0) s.deleteCharAt(s.length() - 1);
        s.append("}\n");
        s.append("bMinusA " + bMinusA.size() + ": {");
        for (int el : bMinusA)
        {
            s.append(el + ",");
        }
        if (bMinusA.size() > 0) s.deleteCharAt(s.length() - 1);
        s.append("}\n");
        return s.toString();
    }
}
