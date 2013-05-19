package org.kahina.core.control;

import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.agent.patterns.TreePatternNode;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;

public class KahinaSimplePropertySensor extends KahinaStepPropertySensor
{
    private KahinaSimpleProperty pattern;
    
    public KahinaSimplePropertySensor(KahinaControlAgent controlPoint)
    {
        this.controlPoint = controlPoint;
        this.pattern = new KahinaSimpleProperty();
    }
    
    public KahinaSimplePropertySensor copy(KahinaControlAgent controlPoint)
    {
        KahinaSimplePropertySensor copy = new KahinaSimplePropertySensor(controlPoint);
        copyDataInto(copy);
        return copy;
    }
    
    public void copyDataInto(KahinaSimplePropertySensor copy)
    {
        copy.pattern = pattern.copy();
    }
    
    public KahinaSimpleProperty getStepProperty()
    {
        return pattern;
    }
    
    /**
     * Gets the step pattern associated with this control point.
     * @return the step pattern associated with this control point
     */
    public KahinaSimpleProperty getPattern()
    {
        return pattern;
    }

    /**
     * Associates this control point with a new step pattern.
     * @param pattern the step pattern to be associated with this control point
     */
    public void setPattern(KahinaSimpleProperty pattern)
    {
        this.pattern = pattern;
    }

    @Override
    public boolean detectPattern(int stepID)
    {
        return pattern.matches(controlPoint.getStepTree(), stepID);
    }
}
