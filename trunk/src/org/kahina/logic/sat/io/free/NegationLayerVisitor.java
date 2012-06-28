package org.kahina.logic.sat.io.free;

import org.kahina.logic.sat.data.free.BooleanConstant;
import org.kahina.logic.sat.data.free.BooleanVariable;
import org.kahina.logic.sat.data.free.Conjunction;
import org.kahina.logic.sat.data.free.Disjunction;
import org.kahina.logic.sat.data.free.Negation;

public class NegationLayerVisitor implements BooleanFormulaVisitor<Integer>
{
    @Override
    public Integer visitConjunction(Conjunction fm)
    {
        return 0;
    }

    @Override
    public Integer visitConstant(BooleanConstant fm)
    {
        return 0;
    }

    @Override
    public Integer visitDisjunction(Disjunction fm)
    {
        return 0;
    }

    @Override
    public Integer visitNegation(Negation fm)
    {
        return 1 + fm.getArg().accept(this);
    }

    @Override
    public Integer visitVariable(BooleanVariable fm)
    {
        return 0;
    }

}
