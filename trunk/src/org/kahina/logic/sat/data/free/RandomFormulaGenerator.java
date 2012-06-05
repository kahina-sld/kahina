package org.kahina.logic.sat.data.free;

import java.util.LinkedList;
import java.util.List;

public class RandomFormulaGenerator
{
    public static BooleanFormula randomFormula(int numVars, int maxHeight, int maxFan, boolean complete)
    {
        double[] fanProb = new double[maxFan + 1];
        fanProb[0] = 0.15;
        fanProb[1] = 0.15;
        for (int i = 2; i <= maxFan; i++)
        {
            fanProb[i] = 0.7 / (maxFan - 1);
        }
        return randomFormula(numVars, maxHeight, fanProb, complete);
    }
    
    public static BooleanFormula randomFormula(int numVars, int maxHeight, double[] fanProb, boolean complete)
    {
        int fan = randomFan(fanProb, maxHeight, complete);
        if (fan == 0)
        {
            //literal
            BooleanVariable var = new BooleanVariable(new VarName((int) (Math.random() * numVars)));
            if (Math.random() > 0.5)
            {
                return new Negation(var);
            }
            else
            {
                return var;
            }
        }
        else if (fan == 1)
        {
            //negation
            return new Negation(randomFormula(numVars, maxHeight - 1, fanProb, complete));
        }
        else
        {
            //conjunction or disjunction
            List<BooleanFormula> fms = new LinkedList<BooleanFormula>();
            for (int i = 0; i < fan; i++)
            {
                fms.add(randomFormula(numVars, maxHeight - 1, fanProb, complete));
            }
            if (Math.random() > 0.5)
            {
                return new Conjunction(fms);
            }
            else
            {
                return new Disjunction(fms);
            }
        }
    }
    
    private static int randomFan(double[] fanProb, int maxHeight, boolean complete)
    {
        if (maxHeight == 1)
        {
            return 0;
        }
        double rnd = Math.random();
        int fan = -1;
        while (rnd > 0.0 && fan < fanProb.length)
        {
            fan++;
            rnd -= fanProb[fan];
        }
        if (complete && fan < 2) return 2;
        return fan;
    }
}
