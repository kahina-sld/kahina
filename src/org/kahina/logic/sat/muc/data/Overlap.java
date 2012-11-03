package org.kahina.logic.sat.muc.data;

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
}
