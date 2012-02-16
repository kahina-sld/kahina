package org.kahina.core.visual.tree;

public class KahinaTreeViewOptions 
{
	// DISPLAY CONSTANTS

	// possible values for shape policies
	public static final int BOX_SHAPE = 0;
	public static final int OVAL_SHAPE = 1;

	// possible values for node display policy
	public static final int ALWAYS = 0;
	public static final int STATUS_DEFAULT_YES = 1;
	public static final int STATUS_DEFAULT_NO = 2;
	public static final int NEVER = 3;
	public static final int CONDITIONALLY = 4; // consults an additional
												// user-definable function to
												// decide

	// possible values for collapsing policy
	public static final int NO_COLLAPSING = 0;
	public static final int COLLAPSE_PRIMARY = 1;
	public static final int COLLAPSE_SECONDARY = 2;

	// possible values for terminals policy
	public static final int NO_SPECIAL_TREATMENT = 0;
	public static final int ON_EXTRA_LEVEL = 1;
	public static final int GRAPHICALLY_SEPARATED = 2;

	// possible values for line display policy
	public static final int STRAIGHT_LINES = 0;
	public static final int EDGY_LINES = 1;
	public static final int INVISIBLE_LINES = 2;

	// possible values for displayOrientation
	public static final int TOP_DOWN_DISPLAY = 0;
	public static final int BOTTOM_UP_DISPLAY = 1;

	// possible values for node position policy
	public static final int CENTERED_NODES = 0;
	public static final int LEFT_ALIGNED_NODES = 1;
	public static final int RIGHT_ALIGNED_NODES = 2;

	// possible values for antialiasing policy
	public static final int ANTIALIASING = 0;
	public static final int NO_ANTIALIASING = 1;

	// possible values for line types
	public static final int COMPLETE_LINES = 0;
	public static final int DOTTED_LINES = 1;
    
    // possible values for cut policy (determines layering behavior)
    public static final int PRIMARY_CUT = 0;
    public static final int SECONDARY_CUT = 1;
    
    // possible values for autoscrolling policy
    public static final int NO_AUTOSCROLL = 0;
    public static final int AUTOSCROLL_TO_MARKED_NODE = 1;
    
    // possible values for edge tag display policy
    public static final int NO_EDGE_TAGS = 0;
    public static final int SIMPLE_EDGE_TAGS = 1;
    public static final int OVAL_EDGE_TAGS = 2;
    public static final int BOXED_EDGE_TAGS = 3;
}
