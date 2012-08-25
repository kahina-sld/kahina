package org.kahina.core.control;

import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.patterns.TreePatternNode;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;

public class KahinaTreePatternSensor extends KahinaStepPropertySensor
{
    private TreePatternNode pattern;
    private KahinaTree tree;
    
    public KahinaTreePatternSensor(KahinaControlPoint controlPoint)
    {
        super(controlPoint);
        this.tree = new KahinaMemTree();
        this.pattern = new TreePatternNode();
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
    
    public KahinaTree getTree()
    {
        return tree;
    }

    public void setTree(KahinaTree tree)
    {
        this.tree = tree;
    }

    public boolean detectPattern(int stepID)
    {
        return pattern.getPattern().matches(tree, stepID);
    }
}
