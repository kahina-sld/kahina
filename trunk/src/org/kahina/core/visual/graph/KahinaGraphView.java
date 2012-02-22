package org.kahina.core.visual.graph;

import java.awt.Color;
import java.awt.Font;
import java.awt.Stroke;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.graph.AdjacListsGraph;
import org.kahina.core.data.graph.KahinaGraph;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;
import org.kahina.core.visual.tree.WidthVector;

public class KahinaGraphView extends KahinaView<KahinaGraph>
{
    protected KahinaGraphViewConfiguration config;
    protected KahinaGraphLayouter layout;
    
    // display coordinates for vertices
    private HashMap<Integer, Integer> xCoord;
    private HashMap<Integer, Integer> yCoord;
    
    // special display properties for certain nodes
    protected HashMap<Integer, Color> vertexBorderColor;
    
    // mappings from status values to display properties (TODO: use all of them)
    HashMap<Integer, Color> statusVertexColorEncoding;
    //HashMap<Integer, Color> statusBorderColorEncoding;
    HashMap<Integer, Color> statusEdgeColorEncoding;
    //HashMap<Integer, Stroke> statusStrokeEncoding;
    HashMap<Integer, Font> statusFontEncoding;
    //HashMap<Integer, Boolean> statusVisibilityEncoding;
    
    public KahinaGraphView(KahinaController control, KahinaGraphLayouter layout)
    {
        super(control);
        this.model = new AdjacListsGraph();
        
        this.config = new KahinaGraphViewConfiguration();
        this.layout = layout;
        
        this.xCoord = new HashMap<Integer,Integer>();
        this.yCoord = new HashMap<Integer,Integer>();
        
        this.vertexBorderColor = new HashMap<Integer,Color>();
        
        this.statusVertexColorEncoding = new HashMap<Integer, Color>();
        //this.statusBorderColorEncoding = new HashMap<Integer, Color>();
        this.statusEdgeColorEncoding = new HashMap<Integer, Color>();
        //this.statusStrokeEncoding = new HashMap<Integer, Stroke>();
        this.statusFontEncoding = new HashMap<Integer, Font>();
        
        layout.newGraph(this);
    }
    
    public void display(KahinaGraph graphModel)
    {
        model = graphModel;
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
    }
    
    public KahinaGraphViewConfiguration getConfig()
    {
        return config;
    }
    
    public void setConfig(KahinaGraphViewConfiguration config)
    {
        this.config = config;
        layout.newGraph(this);
    }
    
    public void setLayouter(KahinaGraphLayouter layouter)
    {
        layout = layouter;
        layout.newGraph(this);
    }
    
    public KahinaGraphLayouter getLayouter()
    {
        return layout;
    }
    
    public Map<Integer, Integer> getXCoordinates()
    {
        return xCoord;
    }
    
    public Map<Integer,Integer> getYCoordinates()
    {
        return yCoord;
    }
    
    public int getDisplayHeight()
    {
        return layout.getDisplayHeight();
    }
    
    public int getDisplayWidth()
    {
        return layout.getDisplayWidth();
    }
    
    public Font getVertexFont(int vertex)
    {
        int status = model.getVertexStatus(vertex);
        Font fnt = statusFontEncoding.get(status);
        if (fnt == null)
        {
            return new Font(Font.SANS_SERIF, Font.PLAIN, config.getZoomLevel());
        } 
        else
        {
            return new Font(fnt.getFamily(), fnt.getStyle(), config.getZoomLevel());
        }
    }
    
    public Color getVertexColor(int nodeID)
    {
        int status = model.getVertexStatus(nodeID);
        Color col = statusVertexColorEncoding.get(status);
        if (col == null)
        {
            return Color.BLACK;
        } 
        else
        {
            return col;
        }
    }

    public void setVertexBorderColor(int nodeID, Color color)
    {
        if (color == null)
        {
            vertexBorderColor.remove(nodeID);
        } 
        else
        {
            vertexBorderColor.put(nodeID, color);
        }
    }

    public Color getVertexBorderColor(int nodeID)
    {
        return vertexBorderColor.get(nodeID);
    }
    
    public Color getEdgeColor(int v1, int v2)
    {
        int status = model.getEdgeStatus(v1,v2);
        Color col = statusVertexColorEncoding.get(status);
        if (col == null)
        {
            return Color.BLACK;
        } 
        else
        {
            return col;
        }
    }
    
    public Integer getVertexX(int vertex)
    {
        return xCoord.get(vertex);
    }

    public Integer getVertexY(int vertex)
    {
        return yCoord.get(vertex);
    }

    public int getVertexHeight(int vertex)
    {
        return 10;
        //TODO: determine this via the font geometry
        //return vertexHeight;
    }
    
    protected void resetLayoutStructures()
    {
        xCoord.clear();
        yCoord.clear();
    }

    @Override
    public JComponent makePanel(KahinaGUI gui)
    {
        KahinaGraphViewPanel panel = new KahinaGraphViewPanel(control);
        control.registerListener("redraw", panel);
        panel.setView(this);
        JScrollPane scrollPane = new JScrollPane(panel);
        scrollPane.getViewport().setBackground(config.getBackgroundColor());
        return scrollPane;
    }
}
