package org.kahina.core.data.breakpoint;

import java.awt.Color;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.breakpoint.patterns.TreeNodePattern;
import org.kahina.core.data.breakpoint.patterns.TreePattern;
import org.kahina.core.io.color.ColorUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class KahinaControlPoint extends KahinaObject
{
    /** a static counter keeping track of the number of breakpoints created so far
     *  only used for default naming purposes */
    static int number = 0;
    private String name;
    private boolean active;
    private Color signalColor;
    private TreeNodePattern pattern;
    //has one of the constant values in KahinaBreakpointType
    private int type;
    
    /**
     * Class constructor specifying the control point type as an integer constant.
     * <p>
     * The breakpoint starts out with the following default values:<br>
     * <code>name</code> - "Breakpoint " + a number<br>
     * <code>signalColor</code> - a random RGB color<br>
     * <code>active</code> - true (= activated)<br>
     * <code>pattern</code> - the empty pattern 
     * @param type one of the constant values in {@link KahinaBreakpointType}
     */
    public KahinaControlPoint(int type)
    {
        number++;
        setName("Control point " + number);
        signalColor = ColorUtil.randomColor();
        active = true;
        pattern = new TreeNodePattern();
        this.type = type;
    }
    
    /**
     * Gets the name of the control point as used by various GUI components.
     * @return the name of the control point
     */
    public String getName()
    {
        return name;
    }

    /**
     * Sets the name of the control point that will be used by various GUI components.
     * @param name a user-readable name for this control point
     */
    public void setName(String name)
    {
        this.name = name;
    }
    
    /**
     * Checks whether this control point is active. 
     * Used by tree automata to decide whether to inform Kahina about matches.
     * @return true if this control point is active, false if it is inactive
     */
    public boolean isActive()
    {
        return active;
    }
    
    /**
     * Activates this control point, causing the messaging system to announce its matches.
     */
    public void activate()
    {
        active = true;
    }
    
    /**
     * Deactivates this control point, preventing the messaging system from announcing its matches.
     */
    public void deactivate()
    {
        active = false;
    }

    /**
     * Gets the signal color used for highlighting matches of this control point.
     * @return the signal color associated with this control point
     */
    public Color getSignalColor()
    {
        return signalColor;
    }

    /**
     * Sets the signal color used for highlighting matches of this control point.
     * @param signalColor the signal color to be associated with this control point
     */
    public void setSignalColor(Color signalColor)
    {
        this.signalColor = signalColor;
    }
    

    /**
     * Returns the breakpoint's name, prefixed by '#' if it is inactive.
     */
    @Override
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
     * Gets the step pattern associated with this control point.
     * @return the step pattern associated with this control point
     */
    public TreeNodePattern getPattern()
    {
        return pattern;
    }

    /**
     * Associates this control point with a new sep pattern.
     * @param pattern the step pattern to be associated with this control point
     */
    public void setPattern(TreeNodePattern pattern)
    {
        this.pattern = pattern;
    }

    /**
     * Gets the type of this control point.
     * @return an integer representing the control point's type
     */
    public int getType()
    {
        return type;
    }

    /**
     * Changes the type of this control point.
     * @param type one of the constant values in {@link KahinaBreakpointType}
     */
    public void setType(int type)
    {
        this.type = type;
    }
    
    /**
     * Generates an XML representation of this control point, optionally featuring an XML header.
     * @param asFile determines whether the result features an XML header
     * @return the XML representation of this control point as a string
     */
    public String exportXML(boolean asFile)
    {
        StringBuilder b = new StringBuilder("");
        if (asFile) b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<controlPoint name=\"" + name + "\" type=\"" + type + "\" color=\"" + ColorUtil.encodeHTML(signalColor) +"\" active=\"" + active + "\">\n");
        b.append(pattern.exportXML(false));
        b.append("</controlPoint>");
        return b.toString();
    }
    
    public Element exportXML(Document dom)
    {
        Element breakpointEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:controlPoint");
        breakpointEl.setAttribute("name", name);
        breakpointEl.setAttribute("type", type + "");
        breakpointEl.setAttribute("color", ColorUtil.encodeHTML(signalColor));
        breakpointEl.setAttribute("active", active + "");
        breakpointEl.appendChild(pattern.exportXML(dom));
        return breakpointEl;
    }
    
    /**
     * Constructs a control point from an XML representation as produced by <code>exportXML</code>.
     * @param controlPointNode an XML DOM element with name "controlPoint" as produced when parsing the result of <code>exportXML</code>
     * @return a new control point object corresponding to the XML representation contained in the DOM element
     */
    public static KahinaControlPoint importXML(Element controlPointNode)
    {
        KahinaControlPoint newControlPoint = new KahinaControlPoint(-1);
        newControlPoint.setName(controlPointNode.getAttribute("name"));
        newControlPoint.setType(Integer.parseInt(controlPointNode.getAttribute("type")));    
        newControlPoint.setSignalColor(ColorUtil.decodeHTML(controlPointNode.getAttribute("color")));
        newControlPoint.active = Boolean.parseBoolean(controlPointNode.getAttribute("active"));
        //expect only one step pattern
        newControlPoint.pattern = TreeNodePattern.importXML((Element) controlPointNode.getElementsByTagName("pattern").item(0));
        return newControlPoint;
    }
}
