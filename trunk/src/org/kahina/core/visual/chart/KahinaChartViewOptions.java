package org.kahina.core.visual.chart;

public class KahinaChartViewOptions 
{
    //DISPLAY CONSTANTS
    
    //possible values for cellWidthPolicy
    public static final int FIXED_WIDTH = 0;
    public static final int MINIMAL_NECESSARY_WIDTH = 1;
    public static final int MAXIMAL_NECESSARY_WIDTH = 2;
    
    //possible values for edgeStackingPolicy
    public static final int STACK_EDGES_FILL_SPACE = 0;
    public static final int STACK_EDGES_BY_ID = 1;
    
    //possible values for displayOrientation
    public static final int BOTTOM_UP_DISPLAY = 0;
    public static final int TOP_DOWN_DISPLAY = 1;
    
    //possible values for displayRangePolicy
    public static final int RANGE_USED_OR_CAPTION_DEFINED = 0;
    public static final int RANGE_USED_ONLY = 1;
    public static final int RANGE_COMPLETE = 2;
    
    //possible values for dependencyDisplayPolicy
    public static final int BOTH_ANCESTORS_AND_DESCENDANTS = 0;
    public static final int ANCESTORS_ONLY = 1;
    public static final int DESCENDANTS_ONLY = 2;
    public static final int NO_DEPENDENCIES = 3;
    
    //possible values for antialiasing policy
    public static final int ANTIALIASING = 0;
    public static final int NO_ANTIALIASING = 1;
}
