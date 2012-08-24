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

    /**
     * Returns true iff a step has the property detected by this sensor. The default sensor never detects a pattern.
     * @param stepID ID of the step which the sensor is to detect for its property.
     * @return true if the sensor's step property is detected, false if it is not.
     */
    public boolean detectPattern(int stepID)
    {
        return false;
    }
}
