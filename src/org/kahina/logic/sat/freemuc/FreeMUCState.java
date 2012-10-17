package org.kahina.logic.sat.freemuc;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaState;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.logic.sat.data.free.BooleanFormula;

public class FreeMUCState extends KahinaState
{
    public static boolean VERBOSE = false;
    
    BooleanFormula formula;
    
    ColoredPathDAG decisionGraph;
    
    public FreeMUCState(KahinaInstance<?,?,?,?> kahina)
    {
        super(kahina);
        this.formula = null;
    }
    
    public FreeMUCState(KahinaInstance<?,?,?,?> kahina, BooleanFormula formula)
    {
        super(kahina);
        this.formula = formula;
    }
    
    public void initialize()
    {
        super.initialize();
        decisionGraph = new ColoredPathDAG();
    }
    
    public void reset()
    {
        this.formula = null;
        initialize();
    }
    
    public BooleanFormula getFormula()
    {
        return formula;
    }
    
    public void setFormula(BooleanFormula formula)
    {
        this.formula = formula;
    }
}
