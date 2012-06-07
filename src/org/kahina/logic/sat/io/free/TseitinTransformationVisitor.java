package org.kahina.logic.sat.io.free;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.free.BooleanConstant;
import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.data.free.BooleanVariable;
import org.kahina.logic.sat.data.free.Conjunction;
import org.kahina.logic.sat.data.free.Disjunction;
import org.kahina.logic.sat.data.free.Negation;
import org.kahina.logic.sat.data.free.VarName;

public class TseitinTransformationVisitor implements BooleanFormulaVisitor<Integer>
{
    Map<BooleanFormula,Integer> tseitinVar;
    private CnfSatInstance cnf;
    
    public TseitinTransformationVisitor()
    {
        tseitinVar = new HashMap<BooleanFormula,Integer>();
        cnf = new CnfSatInstance();
    }
    
    public Integer visitVariable(BooleanVariable fm) 
    {
        tseitinVar.put(fm, fm.getName().num);
        return fm.getName().num;
    }
    
    public Integer visitNegation(Negation fm) 
    {
        Integer xarg = tseitinVar.get(fm.getArg());
        if (xarg == null) 
        {
          xarg = fm.getArg().accept(this);
        }
        Integer x = VarName.freshName();
        tseitinVar.put(fm, x);
        
        List<Integer> clause1 = new LinkedList<Integer>();
        clause1.add(x);
        clause1.add(xarg);
        List<Integer> clause2 = new LinkedList<Integer>();
        clause2.add(-x);
        clause2.add(-xarg);
        cnf.getClauses().add(clause1);
        cnf.getClauses().add(clause2);     
        
        return x;
    }
    
    public Integer visitDisjunction(Disjunction fm) 
    {
        List<Integer> xs = new LinkedList<Integer>();
        for (BooleanFormula f : fm.getDisjuncts()) 
        {
            if (!f.isPruned())
            {
                Integer x = tseitinVar.get(f);
                if (x == null) 
                {
                    x = f.accept(this);
                }
                xs.add(x);
            }
        }
        
        Integer x = VarName.freshName();
        tseitinVar.put(fm, x);

        List<Integer> clause = new LinkedList<Integer>();
        clause.add(-x);
        clause.addAll(xs);
        cnf.getClauses().add(clause);

        for (Integer y : xs) 
        {
            clause = new LinkedList<Integer>();
            clause.add(x);
            clause.add(-y);
            cnf.getClauses().add(clause);
        }

        return x;
    }
    
    public Integer visitConjunction(Conjunction fm) 
    {
        List<Integer> xs = new LinkedList<Integer>();
        for (BooleanFormula f : fm.getConjuncts()) 
        {
            if (!f.isPruned())
            {
                Integer x = tseitinVar.get(f);
                if (x == null) 
                {
                    x = f.accept(this);
                }
                xs.add(x);
            }
        }
        Integer x = VarName.freshName();
        tseitinVar.put(fm, x);

        List<Integer> clause = new LinkedList<Integer>();
        clause.add(x);
        for (Integer y : xs) 
        {
          clause.add(-y);
        }
        cnf.getClauses().add(clause);

        for (Integer y : xs) 
        {
          clause = new LinkedList<Integer>();
          clause.add(-x);
          clause.add(y);
          cnf.getClauses().add(clause);
        }

        return x;
    }

    @Override
    //TODO: constants are simply not covered for now, this must change!
    public Integer visitConstant(BooleanConstant fm)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public CnfSatInstance getCNF(int topVar)
    {
        List<Integer> assertionClause = new LinkedList<Integer>();
        assertionClause.add(topVar);
        cnf.getClauses().add(assertionClause);
        cnf.setNumClauses(cnf.getClauses().size());
        cnf.setNumVars(VarName.freshName() - 1);
        return cnf;
    }
}
