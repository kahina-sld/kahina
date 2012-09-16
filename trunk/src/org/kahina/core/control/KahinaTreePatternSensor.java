package org.kahina.core.control;

import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.patterns.TreePatternNode;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;

public class KahinaTreePatternSensor extends KahinaStepPropertySensor
{
    private KahinaSimpleProperty pattern;
    private KahinaTree stepTree;
    
    public KahinaTreePatternSensor(KahinaControlPoint controlPoint, KahinaTree stepTree)
    {
        this.controlPoint = controlPoint;
        this.stepTree = stepTree;
        this.pattern = new KahinaSimpleProperty();
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
    
    public KahinaTree getStepTree()
    {
        return stepTree;
    }

    public void setStepTree(KahinaTree stepTree)
    {
        this.stepTree = stepTree;
    }

    public boolean detectPattern(int stepID)
    {
        return pattern.matches(stepTree, stepID);
    }
}
