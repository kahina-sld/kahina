package org.kahina.core.breakpoint;

import java.awt.Color;
import java.io.Serializable;

import org.kahina.core.io.color.ColorIO;
import org.w3c.dom.Element;

/**
 * A breakpoint based on a tree pattern as used by the Kahina breakpoint system.
 * <p>
 * The main purpose of this class is to combine a {@info TreePattern} with meta information such as a type, a signal color, a name and an activation status.
 * It constitutes the form in which breakpoints are most transparent and accessible to the user. 
 * The breakpoint editor as well as import and export formats build on breakpoints in this form.
 * <p>
 * Within the breakpoint system, a <code>KahinaBreakpoint</code> is compiled into a {@info TreeAutomaton} which is then used for pattern matching.
 * The resulting <code>TreeAutomaton</code> will however still contain a reference to this object for meta data access.
 * 
 * @author jd
 *
 */
public class KahinaBreakpoint implements Serializable
{
	private static final long serialVersionUID = -6754119800163857876L;
	/** a static counter keeping track of the number of breakpoints created so far
	 *  only used for default naming purposes */
	static int number = 0;
    private String name;
    private boolean active;
    private Color signalColor;
    private TreePattern pattern;
    //has one of the constant values in KahinaBreakpointType
    private int type;
    
    /**
     * Class constructor specifying the breakpoint type as an integer constant.
     * <p>
     * The breakpoint starts out with the following default values:<br>
     * <code>name</code> - "Breakpoint " + a number<br>
     * <code>signalColor</code> - a random RGB color<br>
     * <code>active</code> - true (= activated)<br>
     * <code>pattern</code> - the empty pattern 
     * @param type one of the constant values in {@link KahinaBreakpointType}
     */
    public KahinaBreakpoint(int type)
    {
        number++;
        setName("Breakpoint " + number);
        signalColor = ColorIO.randomColor();
        active = true;
        pattern = new TreePattern();
        this.type = type;
    }
    
    /**
     * Compiles this breakpoint into a tree automaton which is then returned.
     * @return the compiled tree automaton, still referencing this breakpoint
     */
    public TreeAutomaton compile()
    {
        TreeAutomaton a = new TreeAutomaton(this);
        int rootState = compileNode(a, pattern.getRoot());
        a.acceptingStates.add(rootState);
        return a;
    }
    
    //recursive helper method of compile() for single nodes
    private int compileNode(TreeAutomaton a, TreePatternNode node)
    {
        int state = a.states.size();
        a.states.add(state);
        TreeAutomatonRule rule = new TreeAutomatonRule();
        rule.assignedLabel = state;
        rule.pattern = node.getPattern();
        for (TreePatternNode child : node.getChildren())
        {
            rule.requiredChildAnnotations.add(compileNode(a, child));
        }
        a.rules.add(rule);
        return state;
    }
    
    /**
     * Gets the name of the breakpoint as used by various GUI components.
     * @return the name of the breakpoint
     */
    public String getName()
    {
        return name;
    }

    /**
     * Sets the name of the breakpoint that will be used by various GUI components.
     * @param name a user-readable name for this breakpoint
     */
    public void setName(String name)
    {
        this.name = name;
    }
    
    /**
     * Checks whether this breakpoint is active. 
     * Used by tree automata to decide whether to inform Kahina about matches.
     * @return true if this breakpoint is active, false if it is inactive
     */
    public boolean isActive()
    {
        return active;
    }
    
    /**
     * Activates this breakpoint, causing the messaging system to announce its matches.
     */
    public void activate()
    {
        active = true;
    }
    
    /**
     * Deactivates this breakpoint, preventing the messaging system from announcing its matches.
     */
    public void deactivate()
    {
        active = false;
    }

    /**
     * Gets the signal color used for highlighting matches of this breakpoint.
     * @return the signal color associated with this breakpoint
     */
    public Color getSignalColor()
    {
        return signalColor;
    }

    /**
     * Sets the signal color used for highlighting matches of this breakpoint.
     * @param signalColor the signal color to be associated with this breakpoint
     */
    public void setSignalColor(Color signalColor)
    {
        this.signalColor = signalColor;
    }
    
    @Override
    /**
     * Returns the breakpoint's name, prefixed by '#' if it is inactive.
     */
	public String toString()
    {
        if (active)
        {
            return name;
        }
        else
        {
            return "#" + name;
        }
    }

    /**
     * Gets the tree pattern associated with this breakpoint.
     * @return the tree pattern associated with this breakpoint
     */
    public TreePattern getPattern()
    {
        return pattern;
    }

    /**
     * Associates this breakpoint with a new tree pattern.
     * @param pattern the tree pattern to be associated with this breakpoint
     */
    public void setPattern(TreePattern pattern)
    {
        this.pattern = pattern;
    }

    /**
     * Gets the type of this breakpoint.
     * @return an integer representing the breakpoint's type
     */
    public int getType()
    {
        return type;
    }

    /**
     * Changes the type of this breakpoint.
     * @param type one of the constant values in {@link KahinaBreakpointType}
     */
    public void setType(int type)
    {
        this.type = type;
    }
    
    /**
     * Generates an XML representation of this breakpoint, optionally featuring an XML header.
     * @param asFile determines whether the result features an XML header
     * @return the XML representation of this breakpoint as a string
     */
    public String exportXML(boolean asFile)
    {
        StringBuilder b = new StringBuilder("");
        if (asFile) b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<breakpoint name=\"" + name + "\" type=\"" + type + "\" color=\"" + ColorIO.encodeHTML(signalColor) +"\" active=\"" + active + "\">\n");
        b.append(pattern.exportXML(false));
        b.append("</breakpoint>");
        return b.toString();
    }
    
    /**
     * Constructs a breakpoint from an XML representation as produced by <code>exportXML</code>.
     * @param breakpointNode an XML DOM element with name "breakpoint" as produced when parsing the result of <code>exportXML</code>
     * @return a new breakpoint object corresponding to the XML representation contained in the DOM element
     */
    public static KahinaBreakpoint importXML(Element breakpointNode)
    {
        KahinaBreakpoint newBreakpoint = new KahinaBreakpoint(-1);
        newBreakpoint.setName(breakpointNode.getAttribute("name"));
        newBreakpoint.setType(Integer.parseInt(breakpointNode.getAttribute("type")));    
        newBreakpoint.setSignalColor(ColorIO.decodeHTML(breakpointNode.getAttribute("color")));
        newBreakpoint.active = Boolean.parseBoolean(breakpointNode.getAttribute("active"));
        //expect only one tree pattern
        newBreakpoint.pattern = TreePattern.importXML((Element) breakpointNode.getElementsByTagName("treePattern").item(0));
        return newBreakpoint;
    }
}
