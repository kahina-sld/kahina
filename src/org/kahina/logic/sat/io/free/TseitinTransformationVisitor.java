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
        if (!fm.isPruned())
        {
            tseitinVar.put(fm, fm.getName().num);
            return fm.getName().num;
        }
        /*else
        {
            Integer x = VarName.freshName();
            tseitinVar.put(fm, x);
            return x;
        }*/
        return -1;
    }
    
    public Integer visitNegation(Negation fm) 
    {
        if (!fm.isPruned())
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
            cnf.addClause(clause1);
            cnf.addClause(clause2);     
            
            return x;
        }
        /*else
        {
            Integer x = VarName.freshName();
            tseitinVar.put(fm, x);
            return x;
        }*/
        return -1;
    }
    
    public Integer visitDisjunction(Disjunction fm) 
    {
        if (!fm.isPruned())
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
                    if (x != -1)
                    {
                        xs.add(x);
                    }
                }
            }
            
            if (xs.size() > 0)
            {
                Integer x = VarName.freshName();
                tseitinVar.put(fm, x);
        
                List<Integer> clause = new LinkedList<Integer>();
                clause.add(-x);
                clause.addAll(xs);
                cnf.addClause(clause);
        
                for (Integer y : xs) 
                {
                    clause = new LinkedList<Integer>();
                    clause.add(x);
                    clause.add(-y);
                    cnf.addClause(clause);
                }
        
                return x;
            }
        }
        /*else
        {
            Integer x = VarName.freshName();
            tseitinVar.put(fm, x);
            return x;
        }*/
        return -1;
    }
    
    public Integer visitConjunction(Conjunction fm) 
    {
        if (!fm.isPruned())
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
                    if (x != -1)
                    {
                        xs.add(x);
                    }
                }
            }
            
            if (xs.size() > 0)
            {
                Integer x = VarName.freshName();
                tseitinVar.put(fm, x);
                
                List<Integer> clause = new LinkedList<Integer>();
                clause.add(x);
                for (Integer y : xs) 
                {
                  clause.add(-y);
                }
                cnf.addClause(clause);
    
                for (Integer y : xs) 
                {
                  clause = new LinkedList<Integer>();
                  clause.add(-x);
                  clause.add(y);
                  cnf.addClause(clause);
                }
    
                return x;
            }
        }
        /*else
        {
            Integer x = VarName.freshName();
            tseitinVar.put(fm, x);
            return x;
        }*/
        return -1;
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
        cnf.addClause(assertionClause);
        cnf.announceChangedClauses();
        return cnf;
    }
}
