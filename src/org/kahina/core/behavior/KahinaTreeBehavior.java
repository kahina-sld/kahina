package org.kahina.core.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;

/**
 * The ancestor class of all tree behavior classes.
 * <p>
 * A version of {@link KahinaBehavior} for defining the application-specific behavior of {@link KahinaTree}s.
 * Implementations usually define the way a tree is constructed and how it reacts to incoming {@link KahinaEvent}s.
 * A typical use case is {@link org.kahina.lp.behavior.LogicProgrammingTreeBehavior} for control flow tree in logic programming.
 * <p>
 * In an application, the user will usually want to inherit from this in order to define the behavior of tree components.
 * By default, each <code>KahinaBehavior</code> implements the {@link KahinaListener} interface, but it does not register itself with the event system.
 * For that purpose, implementations should use the <code>KahinaRunner.getControl().registerListener</code> mechanism.
 * 
 * @author jd
 */
public class KahinaTreeBehavior extends KahinaBehavior<KahinaTree>
{   
    /**
     * Class constructor specifying the controlled tree and the KahinaInstance that it is connected to.
     * @param tree the KahinaTree the behavior of which is going to be controlled by the new instance
     * @param kahina the KahinaInstance that this behavior will belong to and communicate with
     */
    public KahinaTreeBehavior(KahinaTree tree, KahinaInstance kahina)
    {
        super(tree, kahina);
    }
}
