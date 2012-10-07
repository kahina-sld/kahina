package org.kahina.core.data.agent;

import java.awt.Color;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaSimpleProperty;
import org.kahina.core.control.KahinaStepProperty;
import org.kahina.core.control.KahinaStepPropertySensor;
import org.kahina.core.control.KahinaStepUpdateEvent;
import org.kahina.core.control.KahinaSimplePropertySensor;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.agent.patterns.TreePattern;
import org.kahina.core.data.agent.patterns.TreePatternNode;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.io.color.ColorUtil;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.data.agent.LogicProgrammingControlAgent;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class KahinaControlAgent extends KahinaObject implements KahinaListener
{
    /** a static counter keeping track of the number of breakpoints created so far
     *  only used for default naming purposes */
    static int number = 0;
    
    //
    protected KahinaInstance<?,?,?,?> kahina;
    
    //elementary properties
    protected String name;
    protected boolean active;
    protected Color signalColor;
    
    //interfaces
    protected KahinaStepPropertySensor sensor;
    protected KahinaControlActuator actuator;
    
    /**
     * Class constructor for control points. An actuator must be set before the control point can be used.
     * <p>
     * The breakpoint starts out with the following default values:<br>
     * <code>name</code> - "Breakpoint " + a number<br>
     * <code>signalColor</code> - a random RGB color<br>
     * <code>active</code> - true (= activated)<br>
     * <code>pattern</code> - the empty pattern 
     * @param a backpointer to the relevant KahinaController
     */
    public KahinaControlAgent(KahinaInstance<?,?,?,?> kahina)
    {
        this.kahina = kahina;
        number++;
        setName("Control point " + number);
        signalColor = ColorUtil.randomColor();
        active = true;
        sensor = new KahinaStepPropertySensor(this, new KahinaSimpleProperty());
    }
    
    //this needs to be done before the control agents work!
    public void register()
    {
        kahina.registerSessionListener(KahinaEventTypes.STEP_UPDATE, this);
    }
    
    public void deregister()
    {
        kahina.deregisterSessionListener(KahinaEventTypes.STEP_UPDATE, this);
    }
    
    public KahinaControlAgent copy()
    {
        KahinaControlAgent copy = new KahinaControlAgent(kahina);
        copy.setName(new String(name));
        copy.active = active;
        copy.setSignalColor(new Color(signalColor.getRGB()));
        copy.setSensor(sensor.copy(copy));
        copy.setActuator(actuator);
        return copy;
    }
    
    public KahinaInstance<?,?,?,?> getKahina()
    {
        return kahina;
    }
    
    public KahinaControlActuator getActuator()
    {
        return actuator;
    }

    public void setActuator(KahinaControlActuator actuator)
    {
        this.actuator = actuator;
    }

    public KahinaStepPropertySensor getSensor()
    {
        return sensor;
    }

    public void setSensor(KahinaStepPropertySensor sensor)
    {
        this.sensor = sensor;
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
     * Generates an XML representation of this control point, optionally featuring an XML header.
     * @param asFile determines whether the result features an XML header
     * @return the XML representation of this control point as a string
     */
    public String exportXML(boolean asFile)
    {
        StringBuilder b = new StringBuilder("");
        if (asFile) b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<controlPoint name=\"" + name + "\" color=\"" + ColorUtil.encodeHTML(signalColor) +"\" active=\"" + active + "\">\n");
        b.append(sensor.getStepProperty().exportXML(false));
        b.append("</controlPoint>");
        return b.toString();
    }
    
    public Element exportXML(Document dom)
    {
        Element breakpointEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:controlAgent");
        breakpointEl.setAttribute("name", name);
        breakpointEl.setAttribute("color", ColorUtil.encodeHTML(signalColor));
        breakpointEl.setAttribute("active", active + "");     
        breakpointEl.appendChild(sensor.getStepProperty().exportXML(dom));
        return breakpointEl;
    }
    
    /**
     * Constructs a control point from an XML representation as produced by <code>exportXML</code>.
     * @param controlAgentNode an XML DOM element with name "controlPoint" as produced when parsing the result of <code>exportXML</code>
     * @return a new control point object corresponding to the XML representation contained in the DOM element
     */
    public static KahinaControlAgent importXML(Element controlAgentNode, LogicProgrammingInstance<?,?,?,?> instance)
    {
        KahinaControlAgent newControlPoint = new KahinaControlAgent(instance);
        newControlPoint.setName(controlAgentNode.getAttribute("name"));
        newControlPoint.setSignalColor(ColorUtil.decodeHTML(controlAgentNode.getAttribute("color")));
        newControlPoint.active = Boolean.parseBoolean(controlAgentNode.getAttribute("active"));
        //expect only one step property, TODO: extend this to source location properties
        //find the "pattern" child node, this will be the root element for the pattern
        for (int i = 0; i < controlAgentNode.getChildNodes().getLength(); i++)
        {
            //System.err.println("   Child node of name: " + childNodes.item(i).getNodeName());
            if (controlAgentNode.getChildNodes().item(i).getNodeName().equals("kahina:pattern"))
            {
                //reuse the variable
                controlAgentNode = (Element) controlAgentNode.getChildNodes().item(i);
                break;
            }
        }
        KahinaSimpleProperty property = KahinaSimpleProperty.importXML(controlAgentNode);
        KahinaSimplePropertySensor stepPropertySensor = new KahinaSimplePropertySensor(newControlPoint, instance.getState().getStepTree());
        stepPropertySensor.setPattern(property);
        //TODO: define some useful default sensor here, with focus on making the architecture more easily extendable
        newControlPoint.setSensor(stepPropertySensor);
        return newControlPoint;
    }
    
    public void processEvent(KahinaEvent event)
    {
        if (event instanceof KahinaStepUpdateEvent)
        {
            int stepID = ((KahinaStepUpdateEvent) event).getStepID();
            //check step data against sensor, let the actuator fire if successful
            if (sensor.detectPattern(stepID))
            {
                System.err.println(this + " detected pattern, starting actuator " + actuator);
                actuator.act(this);
            }
        }     
    }
}
