package org.kahina.core.control;

import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.patterns.TreePatternNode;

public class KahinaTreePatternSensor extends KahinaStepPropertySensor
{
    private TreePatternNode pattern;
    
    public KahinaTreePatternSensor(KahinaControlPoint controlPoint)
    {
        super(controlPoint);
        pattern = new TreePatternNode();
    }
    
    /**
     * Gets the step pattern associated with this control point.
     * @return the step pattern associated with this control point
     */
    public TreePatternNode getPattern()
    {
        return pattern;
    }

    /**
     * Associates this control point with a new sep pattern.
     * @param pattern the step pattern to be associated with this control point
     */
    public void setPattern(TreePatternNode pattern)
    {
        this.pattern = pattern;
    }
    
    public boolean detectPattern(int stepID)
    {
        //TODO: compute local pattern match without the detour via a tree automaton
        return false;
    }
}
