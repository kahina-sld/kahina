package org.kahina.logic.sat.data.free;

import org.kahina.logic.sat.io.free.BooleanFormulaVisitor;

public abstract class BooleanFormula
{
    public abstract String toStringWithMinimumBracing();

    public abstract int getSize();
    
    public abstract <A> A accept(BooleanFormulaVisitor<A> visitor);
}
