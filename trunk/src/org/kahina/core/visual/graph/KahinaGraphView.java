package org.kahina.core.visual.graph;

import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.graph.AdjacListsGraph;
import org.kahina.core.data.graph.KahinaGraph;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;

public class KahinaGraphView extends KahinaView<KahinaGraph>
{
    KahinaGraphViewConfiguration config;
    
    // display coordinates for vertices
    private HashMap<Integer, Integer> xCoord;
    private HashMap<Integer, Integer> yCoord;
    
    public KahinaGraphView(KahinaController control)
    {
        super(control);
        model = new AdjacListsGraph();
        
        config = new KahinaGraphViewConfiguration();
        
        xCoord = new HashMap<Integer,Integer>();
        yCoord = new HashMap<Integer,Integer>();
    }
    
    public KahinaGraphViewConfiguration getConfig()
    {
        return config;
    }
    
    public void setConfig(KahinaGraphViewConfiguration config)
    {
        this.config = config;
    }
    
    public Map<Integer, Integer> getXCoordinates()
    {
        return xCoord;
    }
    
    public Map<Integer,Integer> getYCoordinates()
    {
        return yCoord;
    }

    @Override
    public JComponent makePanel(KahinaGUI gui)
    {
        // TODO Auto-generated method stub
        return null;
    }
}
