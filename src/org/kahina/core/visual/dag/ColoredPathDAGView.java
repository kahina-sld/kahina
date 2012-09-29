package org.kahina.core.visual.dag;

import java.awt.Color;
import java.awt.Dimension;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.dag.ColoredPath;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.core.data.dag.KahinaDAG;
import org.kahina.core.gui.KahinaProgressBar;

public class ColoredPathDAGView extends KahinaDAGView
{
    Map<Integer, List<Color>> edgeColors;
    
    ColoredPathDAG model;
    
    public ColoredPathDAGView(KahinaInstance<?, ?, ?, ?> kahina, KahinaDAGLayouter layout)
    {
        super(kahina, layout);
        model = new ColoredPathDAG();
        edgeColors = new HashMap<Integer,List<Color>>();
    }
    
    public ColoredPathDAG getModel()
    {
        return model;
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
            //colors.add(Color.BLACK);
        }
        return colors;
    }
    
    public void recalculate()
    {
        super.recalculate();
        edgeColors.clear();
        for (ColoredPath colorPath : model.getColorPaths())
        {
            List<Integer> path = colorPath.getPath();
            for (int i = 0; i < path.size() - 1; i++)
            {
                int edgeID = model.getEdgeBetween(path.get(i), path.get(i+1));
                addEdgeColor(edgeID,colorPath.getColor());
            }
        }
    }
    
    @Override
    public JComponent makePanel()
    {
        KahinaProgressBar progressBar = new KahinaProgressBar();
        ColoredPathDAGViewPanel panel = new ColoredPathDAGViewPanel(kahina);
        panel.setPreferredSize(new Dimension(200,300));
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        JPanel scrollPaneAndProgressBar = new JPanel();
        JScrollPane scrollPane = new JScrollPane();
        scrollPane.getViewport().setBackground(config.getBackgroundColor());
        scrollPane.setViewportView(panel);
        scrollPaneAndProgressBar.setLayout(new BoxLayout(scrollPaneAndProgressBar, BoxLayout.Y_AXIS));
        scrollPaneAndProgressBar.add(scrollPane);
        scrollPaneAndProgressBar.add(progressBar);
        panel.setProgressBar(progressBar);
        return scrollPaneAndProgressBar;
    }
}
