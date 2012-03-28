package org.kahina.core.visual.graph;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.graph.AdjacListsGraph;
import org.kahina.core.data.graph.KahinaGraph;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaProgressBar;
import org.kahina.core.visual.KahinaView;

public class KahinaGraphView extends KahinaView<KahinaGraph>
{
    protected KahinaGraphViewConfiguration config;
    protected KahinaGraphLayouter layout;
    
    // display coordinates for vertices
    private HashMap<Integer, Integer> xCoord;
    private HashMap<Integer, Integer> yCoord;
    
    // special display properties for certain nodes
    protected HashMap<Integer, Color> vertexBorderColor;
    //depending on vertex visibility policy, everything is visible if this is null
    protected Collection<Integer> visibleVertices;
    //depending on vertex visibility policy, everything is visible if this is null
    protected Collection<Integer> specialVertices;
    
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
    public static final Color MARKING_COLOR = new Color(255, 163, 0);
    
    //redrawing agenda for less time-consuming redraws; also allows to influence the ordering
    //special entry -1 means "redraw everything"
    private List<Integer> redrawAgenda;
    
    public KahinaGraphView(KahinaInstance<?, ?, ?> kahina, KahinaGraphLayouter layout)
    {
        super(kahina);
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
        
        this.redrawAgenda = new LinkedList<Integer>();
        
        layout.newGraph(this);
    }
    
    public void display(KahinaGraph graphModel)
    {
        model = graphModel;
        vertexBorderColor = new HashMap<Integer, Color>();
        this.markedVertex = -1;
        flushRedrawAgenda();
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
        flushRedrawAgenda();
        layout.newGraph(this);
    }
    
    public void setLayouter(KahinaGraphLayouter layouter)
    {
        layout = layouter;
        flushRedrawAgenda();
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
    
    public void setSpecialVertices(Collection<Integer> specVertices)
    {
        specialVertices = specVertices;
    }
    
    public void turnSpecialVerticesBackToNormal()
    {
        specialVertices = null;
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
    
    public List<Integer> getRedrawAgenda()
    {
        return redrawAgenda;
    }
    
    /**
     * Tell the view to redraw the entire structure during the next update.
     */
    public void flushRedrawAgenda()
    {
        redrawAgenda.clear();
        redrawAgenda.add(-1);
    }
    
    public boolean isVertexSpecial(int vertex)
    {
        if (specialVertices == null) return false;
        return specialVertices.contains(vertex);
    }
    
    public boolean isVertexVisible(int vertex)
    {
        switch (config.getVertexVisibilityPolicy())
        {
            case KahinaGraphViewOptions.VERTICES_ALL_VISIBLE:
            {
                return true;
            }
            case KahinaGraphViewOptions.VERTICES_SPECIAL_VISIBLE:
            {
                if (specialVertices == null)
                {
                    return true;
                }
                else
                {
                    return specialVertices.contains(vertex);
                }
            }
            case KahinaGraphViewOptions.VERTICES_EXPLICITLY_VISIBLE:
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
        }
        return false;
    }
    
    public Collection<Integer> getVisibleVertices()
    {
        switch (config.getVertexVisibilityPolicy())
        {
            case KahinaGraphViewOptions.VERTICES_ALL_VISIBLE:
            {
                return model.getVertices();
            }
            case KahinaGraphViewOptions.VERTICES_SPECIAL_VISIBLE:
            {
                if (specialVertices == null)
                {
                    return new LinkedList<Integer>();
                }
                else
                {
                    return specialVertices;
                }
            }
            case KahinaGraphViewOptions.VERTICES_EXPLICITLY_VISIBLE:
            {
                if (visibleVertices == null)
                {
                    return model.getVertices();
                }
                else
                {
                    return visibleVertices;
                }
            }
        }
        return new LinkedList<Integer>();
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
    
    public boolean isEdgeVisible(int vertex1, int vertex2)
    {
        switch (config.getEdgeVisibilityPolicy())
        {
            case KahinaGraphViewOptions.EDGES_ALL_VISIBLE:
            {
                return true;
            }
            case KahinaGraphViewOptions.EDGES_WITH_ONE_NODE_VISIBLE:
            {
                return isVertexVisible(vertex1) || isVertexVisible(vertex2);
            }
            case KahinaGraphViewOptions.EDGES_WITH_BOTH_NODES_VISIBLE:
            {
                return isVertexVisible(vertex1) && isVertexVisible(vertex2);
            }
        }
        return false;
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
    
    public Color getVertexColor(int vertex)
    {
        if (config.getSpecialVertexColoringPolicy() == KahinaGraphViewOptions.SPECIAL_VERTICES_NORMAL_COLOR || isVertexSpecial(vertex))
        {
            int status = model.getVertexStatus(vertex);
            Color col = vertexStatusVertexColorEncoding.get(status);
            if (col == null)
            {       
                col = Color.WHITE;
            }
            if (vertex == markedVertex)
            {
                col = MARKING_COLOR;
            }
            if (col == Color.WHITE && config.getVertexShapePolicy() == KahinaGraphViewOptions.POINT_VERTICES)
            {
                col = Color.BLACK;
            }
            return col;
        }
        return Color.gray;
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
        if (markedVertex != -1) redrawAgenda.add(markedVertex);
        if (vertex != -1) redrawAgenda.add(vertex);
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
        switch (config.getEdgeColoringPolicy())
        {
            case KahinaGraphViewOptions.EDGE_COLOR_INDEPENDENT:
            {
                int status = model.getEdgeStatus(v1,v2);
                Color col = edgeStatusEdgeColorEncoding.get(status);
                if (col == null) return Color.gray;
                return col;
            }
            case KahinaGraphViewOptions.EDGE_COLOR_FUNCTION_OF_VERTEX_COLOR:
            {
                int v1status = model.getVertexStatus(v1);
                int v2status = model.getVertexStatus(v2);
                Color col = vertexStatusEdgeColorEncoding.get(v1status + "" + v2status);
                if (col == null) return Color.gray;
                return col;
            }
            case KahinaGraphViewOptions.EDGE_COLOR_BETWEEN_NODES_OF_SAME_COLOR:
            {
                Color v1color = getVertexColor(v1);
                Color v2color = getVertexColor(v2);
                if (v1color.equals(v2color)) return v1color;
                return Color.black;
            }
        }
        return Color.black;
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
    
    public void exportVisibleSubgraphTGF(String fileName) throws IOException
    {
        try
        {
            BufferedWriter out = new BufferedWriter(new FileWriter(fileName));
            //print vertex IDs and vertex labels of visible vertices
            for (int vertex : model.getVertices())
            {
                if (isVertexVisible(vertex))
                {
                    out.write(vertex + " " + model.getVertexLabel(vertex) + "\n");
                }
            }
            out.write("#\n");
            //print edges between visible vertices
            for (int vertex1 : model.getVertices())
            {
                if (isVertexVisible(vertex1))
                {
                    for (int vertex2 : model.getNeighbors(vertex1))
                    {
                        if (isVertexVisible(vertex2))
                        {
                            out.write(vertex1 + " " + vertex2 + " " + model.getEdgeLabel(vertex1,vertex2) + "\n");
                        }
                    }
                }
            }
            out.close();
        }
        catch (FileNotFoundException e)
        {
            System.err.println("ERROR: File for TGF output not found. Aborting TGF output!");
        }
    }

    @Override
    public JComponent makePanel()
    {
        KahinaProgressBar progressBar = new KahinaProgressBar();
        KahinaGraphViewPanel panel = new KahinaGraphViewPanel(kahina);
        panel.setPreferredSize(new Dimension(200,300));
        kahina.getGuiControl().registerListener("redraw", panel);
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
