package org.kahina.core.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;

/**
 * The generic ancestor class of all behavior classes.
 * <p>
 * Behaviors are Kahina's way of defining the application-specific behavior of complex components such as trees.
 * They usually determine how a complex object is constructed and how it reacts to incoming {@link KahinaEvent}s.
 * A typical use case is {@link org.kahina.lp.behavior.LogicProgrammingTreeBehavior} for control flow tree in logic programming.
 * <p>
 * This class is generic and can be specialized for any subclass of {@link KahinaObject}.
 * <p>
 * In an application, the user will usually want to inherit from a specialized variant such as {@link KahinaTreeBehavior}.
 * By default, a KahinaBehavior implements the {@link KahinaListener} interface, but it does not register itself with the event system.
 * For that purpose, implementations should use the {@link KahinaRunner.getControl().registerListener()} mechanism.
 * 
 * @author jd
 *
 * @param <T> a subclass of {@link KahinaObject}
 */
public class KahinaBehavior<T extends KahinaObject> implements KahinaListener
{
    protected T object;
    // the Kahina instance this behavior is connected with
    protected KahinaInstance kahina;
    
    /**
     * Class constructor specifying the controlled object and the KahinaInstance that uses it
     * @param object the KahinaObject of type T that is going to be controlled by the new instance
     * @param kahina the KahinaInstance that this behavior will belong to and communicate with
     */
    public KahinaBehavior(T object, KahinaInstance kahina)
    {
        this.object = object;
        this.kahina = kahina;
    }
    
    /**
     * does not react to any event by default; is overridden by implementations
     */
    public void processEvent(KahinaEvent e)
    {
        
    }
}
