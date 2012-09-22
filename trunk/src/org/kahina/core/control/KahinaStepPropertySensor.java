package org.kahina.core.control;

import org.kahina.core.data.breakpoint.KahinaControlAgent;

public class KahinaStepPropertySensor
{
    KahinaControlAgent controlPoint;
    
    //the property this sensor is looking for
    KahinaStepProperty stepProperty;
    
    protected KahinaStepPropertySensor()
    {
        
    }
    
    public KahinaStepPropertySensor(KahinaControlAgent controlPoint, KahinaStepProperty stepProperty)
    {
        this.controlPoint = controlPoint;
        this.stepProperty = stepProperty;
    }
    
    public KahinaStepPropertySensor copy(KahinaControlAgent controlPoint)
    {
        KahinaStepPropertySensor copy = new KahinaStepPropertySensor(controlPoint, null);
        copyDataInto(copy);
        return copy;
    }
    
    public void copyDataInto(KahinaStepPropertySensor copy)
    {
        copy.stepProperty = stepProperty.copy();
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
        System.err.println("WARNING: non-overridden call KahinaStepPropertySensor.detectPattern(" + stepID + ") = false");
        return false;
    }


}
