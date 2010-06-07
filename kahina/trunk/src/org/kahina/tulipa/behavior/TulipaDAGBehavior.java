package org.kahina.tulipa.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.behavior.KahinaDAGBehavior;
import org.kahina.core.data.dag.KahinaDAG;

public class TulipaDAGBehavior extends KahinaDAGBehavior
{
    public TulipaDAGBehavior(KahinaDAG dag, KahinaInstance kahina)
    {
        super(dag, kahina);
        KahinaRunner.getControl().registerListener("tulipa bridge", this);
    }
}
