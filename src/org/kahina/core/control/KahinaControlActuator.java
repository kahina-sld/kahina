package org.kahina.core.control;

public class KahinaControlActuator
{
    protected KahinaController control;
    
    public KahinaControlActuator(KahinaController control)
    {
        this.control = control;
    }
    
    /**
     * Causes this actuator to execute its action (usually sending out a control event).
     * The default actuator does nothing.
     */
    public void act()
    {
        
    }
}
