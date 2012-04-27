package org.kahina.core.data.dag;

import java.util.LinkedList;
import java.util.List;

public class ColoredPathDAG extends KahinaMemDAG
{
    /**
     * 
     */
    private static final long serialVersionUID = -5409083393589686558L;

    private List<ColoredPath> colorPaths;
    
    public ColoredPathDAG()
    {
        super();
        colorPaths = new LinkedList<ColoredPath>();
    }
    
    public void addColorPath(ColoredPath path)
    {
        colorPaths.add(path);
    }
    
    public boolean removeColorPath(ColoredPath path)
    {
        return colorPaths.remove(path);
    }

    public List<ColoredPath> getColorPaths()
    {
        return colorPaths;
    }
}
