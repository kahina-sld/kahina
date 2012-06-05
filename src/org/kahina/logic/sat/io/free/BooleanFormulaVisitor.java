package org.kahina.logic.sat.io.free;

import org.kahina.logic.sat.data.free.BooleanConstant;
import org.kahina.logic.sat.data.free.BooleanVariable;
import org.kahina.logic.sat.data.free.Conjunction;
import org.kahina.logic.sat.data.free.Disjunction;
import org.kahina.logic.sat.data.free.Negation;

public abstract interface BooleanFormulaVisitor<T>
{
    public abstract T visitConstant(BooleanConstant fm);
    
    public abstract T visitVariable(BooleanVariable fm);

    public abstract T visitNegation(Negation fm);

    public abstract T visitDisjunction(Disjunction fm);

    public abstract T visitConjunction(Conjunction fm);
}
