package org.kahina.core.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.dag.KahinaDAG;

/**
 * The ancestor class of all DAG behavior classes.
 * <p>
 * A version of {@link KahinaBehavior} for defining the application-specific behavior of {@link KahinaDAG}s.
 * Implementations usually define the way a DAG is constructed and how it reacts to incoming {@link KahinaEvent}s.
 * A typical use case is {@link org.kahina.tulipa.behavior.TulipaDAGBehavior} for the search space visualization in the TuLiPA parsing system.
 * <p>
 * In an application, the user will usually want to inherit from this in order to define the behavior of DAG components.
 * By default, each <code>KahinaBehavior</code> implements the {@link KahinaListener} interface, but it does not register itself with the event system.
 * For that purpose, implementations should use the <code>KahinaRunner.getControl().registerListener</code> mechanism.
 * 
 * @author jd
 */
public class KahinaDAGBehavior extends KahinaBehavior<KahinaDAG> 
{
    /**
     * Class constructor specifying the controlled DAG and the KahinaInstance that it is connected to.
     * @param dag the KahinaDAG the behavior of which is going to be controlled by the new instance
     * @param kahina the KahinaInstance that this behavior will belong to and communicate with
     */
    public KahinaDAGBehavior(KahinaDAG dag, KahinaInstance kahina)
    {
        super(dag, kahina);
    }
}
