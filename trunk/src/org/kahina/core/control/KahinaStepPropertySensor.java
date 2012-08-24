package org.kahina.core.control;

import org.kahina.core.data.breakpoint.KahinaControlPoint;

public class KahinaStepPropertySensor
{
    KahinaControlPoint controlPoint;
    
    //the property this sensor is looking for
    KahinaStepProperty stepProperty;
    
    public KahinaStepPropertySensor(KahinaControlPoint controlPoint)
    {
        this.controlPoint = controlPoint;
    }
    
    public KahinaStepProperty getStepProperty()
    {
        return stepProperty;
    }
}
