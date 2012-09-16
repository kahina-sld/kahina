package org.kahina.core.control;

import org.kahina.core.data.breakpoint.KahinaControlAgent;

public class KahinaControlActuator
{
    protected KahinaController control;
    
    protected KahinaControlActuator(KahinaController control)
    {
        this.control = control;
    }
    
    /**
     * Causes this actuator to execute its action (usually sending out a control event).
     * The default actuator does nothing.
     */
    public void act(KahinaControlAgent agent)
    {
        
    }
    
    public KahinaController getControl()
    {
        return control;
    }
}
