package org.kahina.lp.data.agent;

import org.kahina.core.control.KahinaSimpleProperty;
import org.kahina.core.control.KahinaSimplePropertySensor;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.io.color.ColorUtil;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.w3c.dom.Element;

public class LogicProgrammingControlAgent extends KahinaControlAgent
{
	private static final long serialVersionUID = 915254841399696437L;
	
	private static final boolean VERBOSE = false;
    protected KahinaSimplePropertySensor sensor;
    
    public LogicProgrammingControlAgent(LogicProgrammingInstance<?,?,?,?> kahina)
    {
        super(kahina);

    	if (VERBOSE)
    	{
    		System.err.println(this + ".(" + kahina + ")");
    	}
        setSensor(new KahinaSimplePropertySensor(this));
    }
    
    public KahinaTree getStepTree()
    {
    	LogicProgrammingState state = kahina.getState();
        return state.getStepTree();
    }
    
    public KahinaSimplePropertySensor getSensor()
    {
        return sensor;
    }
    
    public void setSensor(KahinaSimplePropertySensor sensor)
    {
        super.setSensor(sensor);
        this.sensor = sensor;
    }
    
    /**
     * Constructs a logic programming control point from an XML representation as produced by <code>exportXML</code>.
     * @param controlPointNode an XML DOM element with name "controlPoint" as produced when parsing the result of <code>exportXML</code>
     * @return a new logic programming control point object corresponding to the XML representation contained in the DOM element
     */
    public static LogicProgrammingControlAgent importXML(Element controlPointNode, LogicProgrammingInstance<?,?,?,?> instance)
    {
        LogicProgrammingControlAgent newControlPoint = new LogicProgrammingControlAgent(instance);
        newControlPoint.setName(controlPointNode.getAttribute("name"));
        newControlPoint.setSignalColor(ColorUtil.decodeHTML(controlPointNode.getAttribute("color")));
        newControlPoint.active = Boolean.parseBoolean(controlPointNode.getAttribute("active"));
        //expect only one tree pattern
        KahinaSimplePropertySensor treePatternSensor = new KahinaSimplePropertySensor(newControlPoint);
        treePatternSensor.setPattern(KahinaSimpleProperty.importXML((Element) controlPointNode.getElementsByTagName("kahina:pattern").item(0)));
        newControlPoint.setSensor(treePatternSensor);
        return newControlPoint;
    }
}
