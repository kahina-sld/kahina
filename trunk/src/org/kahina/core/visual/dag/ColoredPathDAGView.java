package org.kahina.core.visual.dag;

import java.awt.Color;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.dag.ColoredPath;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.core.data.dag.KahinaDAG;

public class ColoredPathDAGView extends KahinaDAGView
{
    Map<Integer, List<Color>> edgeColors;
    
    ColoredPathDAG model;
    
    public ColoredPathDAGView(KahinaInstance<?, ?, ?> kahina, KahinaDAGLayouter layout)
    {
        super(kahina, layout);
        edgeColors = new HashMap<Integer,List<Color>>();
    }
    
    public void display(ColoredPathDAG dagModel)
    {
        super.display(dagModel);
        this.model = dagModel;
    }
    
    private void addEdgeColor(int edgeID, Color color)
    {
        List<Color> colors = edgeColors.get(edgeID);
        if (colors == null)
        {
            colors = new LinkedList<Color>();
            edgeColors.put(edgeID, colors);
        }
        colors.add(color);
    }

    public List<Color> getEdgeColors(int edgeID)
    {
        List<Color> colors = edgeColors.get(edgeID);
        if (colors == null)
        {
            colors = new LinkedList<Color>();
            colors.add(Color.BLACK);
        }
        return colors;
    }
    
    public void recalculate()
    {
        super.recalculate();
        for (ColoredPath colorPath : model.getColorPaths())
        {
            List<Integer> path = colorPath.getPath();
            for (int i = 0; i < path.size() - 1; i++)
            {
                //TODO: implement getEdgeID
                //int edgeID = model.getEdgeID(path.get(i), path.get(i+1));
                //addEdgeColor(edgeID,colorPath.getColor());
            }
        }
    }
}
