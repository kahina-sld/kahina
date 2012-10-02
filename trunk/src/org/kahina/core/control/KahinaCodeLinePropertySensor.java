package org.kahina.core.control;

import org.kahina.core.KahinaState;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.agent.patterns.TreePatternNode;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.lp.LogicProgrammingState;

public class KahinaCodeLinePropertySensor extends KahinaStepPropertySensor
{
    private KahinaCodeLineProperty line;
    private KahinaState state;
    
    public KahinaCodeLinePropertySensor(KahinaControlAgent controlPoint, KahinaState state, KahinaCodeLineProperty line)
    {
        this.controlPoint = controlPoint;
        this.state = state;
        this.line = line;
    }
    
    public KahinaCodeLinePropertySensor copy(KahinaControlAgent controlPoint)
    {
        KahinaCodeLinePropertySensor copy = new KahinaCodeLinePropertySensor(controlPoint, state, null);
        copyDataInto(copy);
        return copy;
    }
    
    public void copyDataInto(KahinaCodeLinePropertySensor copy)
    {
        copy.line = line.copy();
    }
    
    public KahinaCodeLineProperty getStepProperty()
    {
        return line;
    }
    
    /**
     * Gets the source code location associated with this control point.
     * @return the source code location that this control point reacts to.
     */
    public KahinaSourceCodeLocation getCodeLocation()
    {
        return line.location;
    }

    /**
     * Associates this control point with a source code location.
     * @param pattern the step pattern to be associated with this control point
     */
    public void setCodeLocation(KahinaSourceCodeLocation location)
    {
        this.line = new KahinaCodeLineProperty(location);
    }
    
    public KahinaState getState()
    {
        return state;
    }

    public void setState(LogicProgrammingState state)
    {
        this.state = state;
    }

    public boolean detectPattern(int stepID)
    {
        return line.matches(state.get(stepID).getSourceCodeLocation());
    }
}
