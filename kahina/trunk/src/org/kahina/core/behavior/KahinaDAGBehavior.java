package org.kahina.core.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.dag.KahinaDAG;
import org.kahina.core.event.KahinaEvent;

public class KahinaDAGBehavior extends KahinaBehavior<KahinaDAG> implements KahinaListener
{
    public KahinaDAGBehavior(KahinaDAG dag, KahinaInstance kahina)
    {
        super(dag, kahina);
        KahinaRunner.getControl().registerListener("dag", this);
    }
    
    public void processEvent(KahinaEvent e)
    {
        
    }
}
