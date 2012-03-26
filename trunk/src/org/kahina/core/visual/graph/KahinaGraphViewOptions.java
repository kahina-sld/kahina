package org.kahina.core.visual.graph;

public class KahinaGraphViewOptions
{
 // DISPLAY CONSTANTS

    // possible values for vertex shape policy
    public static final int POINT_VERTICES = 0;
    public static final int BOX_VERTICES = 1;
    public static final int OVAL_VERTICES = 2;
    
    // possible values for edge label display policy
    public static final int NO_EDGE_LABELS = 0;
    public static final int SIMPLE_EDGE_LABELS = 1;
    public static final int OVAL_EDGE_LABELS = 2;
    public static final int BOXED_EDGE_LABELS = 3;
    
    // possible values for drawing order policy
    public static final int VERTICES_ABOVE_EDGES = 0;
    public static final int EDGES_ABOVE_VERTICES = 1;
    
    //possible values for edge shape policy
    public static final int EDGE_SHAPE_DIRECT = 0;
    public static final int EDGE_SHAPE_RECTANGULAR = 1;
    public static final int EDGE_SHAPE_ARC = 2;
    
    // possible values for antialiasing policy
    public static final int ANTIALIASING = 0;
    public static final int NO_ANTIALIASING = 1;
    
    // possible values for graph layout option
    public static final int LAYOUT_GRID = 0;
    public static final int LAYOUT_CIRCULAR = 1;
    public static final int LAYOUT_SPRING = 2;
    
    //possible values for vertex visibility policy
    public static final int VERTICES_ALL_VISIBLE = 0;
    public static final int VERTICES_SPECIAL_VISIBLE = 1;
    public static final int VERTICES_EXPLICITLY_VISIBLE = 2;
    
    //possible values for edge visibility policy
    public static final int EDGES_ALL_VISIBLE = 0;
    public static final int EDGES_WITH_ONE_NODE_VISIBLE = 1;
    public static final int EDGES_WITH_BOTH_NODES_VISIBLE = 2;
    
    //possible values for edge coloring policy
    public static final int EDGE_COLOR_INDEPENDENT = 0;
    public static final int EDGE_COLOR_FUNCTION_OF_VERTEX_COLOR = 1;
    public static final int EDGE_COLOR_BETWEEN_NODES_OF_SAME_COLOR = 2;
    
    //possible values for special vertex position
    public static final int SPECIAL_VERTICES_SEPARATE = 0;
    public static final int SPECIAL_VERTICES_MIXED = 1;
    
    //possible values for special vertex coloring
    public static final int SPECIAL_VERTICES_HIGHLIGHTED = 0;
    public static final int SPECIAL_VERTICES_NORMAL_COLOR = 1;
    
    //possible values for edge interpretation
    public static final int EDGE_INTERPRETATION_DIRECTED = 0;
    public static final int EDGE_INTERPRETATION_UNDIRECTED = 1;
}
