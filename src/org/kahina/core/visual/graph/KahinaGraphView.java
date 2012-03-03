package org.kahina.core.visual.graph;

import java.awt.Color;
import java.awt.Font;
import java.awt.Stroke;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
    protected Collection<Integer> visibleVertices; //everything is visible if this is null
    
    // mappings from status values to display properties (TODO: use all of them)
    HashMap<Integer, Color> vertexStatusVertexColorEncoding;
    HashMap<String, Color> vertexStatusEdgeColorEncoding;
    //HashMap<Integer, Color> statusBorderColorEncoding;
    HashMap<Integer, Color> edgeStatusEdgeColorEncoding;
    //HashMap<Integer, Stroke> statusStrokeEncoding;
    HashMap<Integer, Font> statusFontEncoding;
    //HashMap<Integer, Boolean> statusVisibilityEncoding;
    
    //allow one vertex to be marked
    private int markedVertex;
    public static final Color MARKING_COLOR = Color.ORANGE;
    
    public KahinaGraphView(KahinaController control, KahinaGraphLayouter layout)
    {
        super(control);
        this.model = new AdjacListsGraph();
        
        this.config = new KahinaGraphViewConfiguration();
        this.layout = layout;
        
        this.xCoord = new HashMap<Integer,Integer>();
        this.yCoord = new HashMap<Integer,Integer>();
        
        this.vertexBorderColor = new HashMap<Integer,Color>();
        
        this.vertexStatusVertexColorEncoding = new HashMap<Integer, Color>();
        this.vertexStatusEdgeColorEncoding = new HashMap<String, Color>();
        //this.statusBorderColorEncoding = new HashMap<Integer, Color>();
        this.edgeStatusEdgeColorEncoding = new HashMap<Integer, Color>();
        //this.statusStrokeEncoding = new HashMap<Integer, Stroke>();
        this.statusFontEncoding = new HashMap<Integer, Font>();
        
        this.markedVertex = -1;
        
        layout.newGraph(this);
    }
    
    public void display(KahinaGraph graphModel)
    {
        model = graphModel;
        vertexBorderColor = new HashMap<Integer, Color>();
        this.markedVertex = -1;
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
    
    public void setVisibleVertices(Collection<Integer> visVertices)
    {
        visibleVertices = visVertices;
    }
    
    public void setAllVisible()
    {
        visibleVertices = null;
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
    
    public boolean isVertexVisible(int vertex)
    {
        if (visibleVertices == null)
        {
            return true;
        }
        else
        {
            return visibleVertices.contains(vertex);
        }
    }
    
    public List<Integer> getVisibleNeighbors(int vertex)
    {
        List<Integer> visNeighbors = new LinkedList<Integer>();
        for (int neighbor : model.getNeighbors(vertex))
        {
            if (isVertexVisible(neighbor))
            {
                visNeighbors.add(neighbor);
            }
        }
        return visNeighbors;
    }
    
    public Font getVertexFont(int vertex)
    {
        int status = model.getVertexStatus(vertex);
        Font fnt = statusFontEncoding.get(status);
        if (fnt == null)
        {
            return new Font(Font.SANS_SERIF, Font.PLAIN, config.getNodeSize());
        } 
        else
        {
            return new Font(fnt.getFamily(), fnt.getStyle(), config.getNodeSize());
        }
    }
    
    public Color getVertexColor(int nodeID)
    {
        int status = model.getVertexStatus(nodeID);
        Color col = vertexStatusVertexColorEncoding.get(status);
        if (col == null)
        {       
            col = Color.WHITE;
        }
        if (nodeID == markedVertex)
        {
            col = MARKING_COLOR;
        }
        if (col == Color.WHITE && config.getVertexShapePolicy() == KahinaGraphViewOptions.POINT_VERTICES)
        {
            col = Color.BLACK;
        }
        return col;
    }
    
    public void setVertexStatusVertexColorEncoding(int state, Color color)
    {
        vertexStatusVertexColorEncoding.put(state, color);
    }
    
    public void setVertexStatusEdgeColorEncoding(int v1state, int v2state, Color color)
    {
        vertexStatusEdgeColorEncoding.put(v1state + "" + v2state, color);
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
    
    public void setMarkedVertex(int vertex)
    {
        markedVertex = vertex;
    }
    
    public int getMarkedVertex()
    {
        return markedVertex;
    }
    
    public Color getEdgeColor(int v1, int v2)
    {
        if (v1 == markedVertex || v2 == markedVertex)
        {
           return MARKING_COLOR;
        }
        int status = model.getEdgeStatus(v1,v2);
        Color col = edgeStatusEdgeColorEncoding.get(status);
        if (col == null)
        {
            int v1status = model.getVertexStatus(v1);
            int v2status = model.getVertexStatus(v2);
            col = vertexStatusEdgeColorEncoding.get(v1status + "" + v2status);
            if (col == null) col = Color.black;
        } 
        return col;
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
        return config.getNodeSize();
    }
    
    protected void resetLayoutStructures()
    {
        xCoord.clear();
        yCoord.clear();
        visibleVertices = null;
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
