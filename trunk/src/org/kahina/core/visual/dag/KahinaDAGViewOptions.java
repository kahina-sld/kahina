package org.kahina.core.visual.dag;

public class KahinaDAGViewOptions
{
    // possible values for vertex shape policy
    public static final int POINT_VERTICES = 0;
    public static final int BOX_VERTICES = 1;
    public static final int OVAL_VERTICES = 2;
    
    // possible values for edge label display policy
    public static final int NO_EDGE_LABELS = 0;
    public static final int SIMPLE_EDGE_LABELS = 1;
    public static final int OVAL_EDGE_LABELS = 2;
    public static final int BOXED_EDGE_LABELS = 3;
    
    // possible values for antialiasing policy
    public static final int ANTIALIASING = 0;
    public static final int NO_ANTIALIASING = 1;
    
    // possible values for root position policy
    public static final int ROOT_POSITION_FIRST_LINE = 0;
    public static final int ROOT_POSITION_DEEP = 1;
}
