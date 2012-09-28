package org.kahina.core.control;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.agent.KahinaControlAgent;

public class KahinaControlActuator
{
    protected KahinaInstance<?,?,?,?> kahina;
    
    protected KahinaControlActuator(KahinaInstance<?,?,?,?> kahina)
    {
        this.kahina = kahina;
    }
    
    /**
     * Causes this actuator to execute its action (usually sending out a control event).
     * The default actuator does nothing.
     */
    public void act(KahinaControlAgent agent)
    {
        
    }
    
    public KahinaInstance<?,?,?,?> getKahina()
    {
        return kahina;
    }
}
