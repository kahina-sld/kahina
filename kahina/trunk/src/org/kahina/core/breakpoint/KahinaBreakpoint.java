package org.kahina.core.breakpoint;

import java.awt.Color;
import java.io.File;

import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.data.chart.KahinaDbChart;
import org.kahina.core.data.chart.KahinaMemChart;
import org.kahina.core.io.color.ColorIO;
import org.kahina.core.io.database.DatabaseHandler;
import org.kahina.core.io.util.XMLUtilities;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class KahinaBreakpoint
{
    static int number = 0;
    private String name;
    private boolean active;
    private Color signalColor;
    private TreePattern pattern;
    //has one of the constant values in KahinaBreakpointType
    private int type;
    
    public KahinaBreakpoint(int type)
    {
        number++;
        setName("Breakpoint " + number);
        signalColor = randomColor();
        active = true;
        pattern = new TreePattern();
        this.type = type;
    }
    
    public TreeAutomaton compile()
    {
        TreeAutomaton a = new TreeAutomaton(this);
        int rootState = compileNode(a, pattern.getRoot());
        a.acceptingStates.add(rootState);
        return a;
    }
    
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
    
    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }
    
    public boolean isActive()
    {
        return active;
    }
    
    public void activate()
    {
        active = true;
    }
    
    public void deactivate()
    {
        active = false;
    }

    public Color getSignalColor()
    {
        return signalColor;
    }

    public void setSignalColor(Color signalColor)
    {
        this.signalColor = signalColor;
    }
    
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

    public TreePattern getPattern()
    {
        return pattern;
    }

    public void setPattern(TreePattern pattern)
    {
        this.pattern = pattern;
    }
    
    public Color randomColor()
    {
        int r = (int) (Math.random() * 256);
        int g = (255 - r) + (int) (Math.random() * r);
        int b = 510 - r - g;
        return new Color(r,g,b);
    }

    public int getType()
    {
        return type;
    }

    public void setType(int type)
    {
        this.type = type;
    }
    
    public String exportXML(boolean asFile)
    {
        StringBuilder b = new StringBuilder("");
        if (asFile) b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<breakpoint name=\"" + name + "\" type=\"" + type + "\" color=\"" + ColorIO.encodeHTML(signalColor) +"\" active=\"" + active + "\">\n");
        b.append(pattern.exportXML(false));
        b.append("</breakpoint>");
        return b.toString();
    }
    
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
    
    /*public static KahinaBreakpoint importXML(Document dom, int dataHandlingMethod, DatabaseHandler db)
    {
        
        KahinaBreakpoint m = new KahinaBreakpoint();
        if (dataHandlingMethod == KahinaDataHandlingMethod.MEMORY)
        {
            m = new KahinaMemChart();
        }
        else
        {
            m = new KahinaDbChart();
        }
        NodeList segments = dom.getElementsByTagName("segment");
        for (int i = 0; i < segments.getLength(); i++)
        {
            Element segment = (Element) segments.item(i);
            int id = Integer.parseInt(segment.getAttribute("id"));
            m.setSegmentCaption(id,segment.getAttribute("caption"));
        }
        NodeList edges = dom.getElementsByTagName("edge");
        for (int i = 0; i < edges.getLength(); i++)
        {
            Element edge = (Element) edges.item(i);
            int id = Integer.parseInt(edge.getAttribute("id"));
            int left = Integer.parseInt(edge.getAttribute("left"));
            int right = Integer.parseInt(edge.getAttribute("right"));
            int status = Integer.parseInt(edge.getAttribute("status"));
            if (left < m.getLeftmostCovered()) m.setLeftmostCovered(left);
            if (right > m.getRightmostCovered()) m.setRightmostCovered(right);
            m.setLeftBoundForEdge(id, left);
            m.setRightBoundForEdge(id, right);
            m.setEdgeStatus(id, status);
            m.setEdgeCaption(id,edge.getAttribute("caption"));
        }
        return m;
    }*/
}
