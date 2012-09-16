package org.kahina.lp.data.breakpoint;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaSimpleProperty;
import org.kahina.core.control.KahinaStepPropertySensor;
import org.kahina.core.control.KahinaSimplePropertySensor;
import org.kahina.core.data.breakpoint.KahinaControlAgent;
import org.kahina.core.data.breakpoint.patterns.TreePatternNode;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.io.color.ColorUtil;
import org.kahina.lp.LogicProgrammingState;
import org.w3c.dom.Element;

public class LogicProgrammingControlAgent extends KahinaControlAgent
{
    protected KahinaSimplePropertySensor sensor;
    protected KahinaTree stepTree;
    
    public LogicProgrammingControlAgent(KahinaController control, KahinaTree stepTree)
    {
        super(control);
        this.stepTree = stepTree;
        setSensor(new KahinaSimplePropertySensor(this, stepTree));
    }
    
    public KahinaTree getStepTree()
    {
        return stepTree;
    }
    
    public void setStepTree(KahinaTree stepTree)
    {
        this.stepTree = stepTree;
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
    public static LogicProgrammingControlAgent importXML(Element controlPointNode, KahinaController control, KahinaTree stepTree)
    {
        LogicProgrammingControlAgent newControlPoint = new LogicProgrammingControlAgent(control, stepTree);
        newControlPoint.setName(controlPointNode.getAttribute("name"));
        newControlPoint.setSignalColor(ColorUtil.decodeHTML(controlPointNode.getAttribute("color")));
        newControlPoint.active = Boolean.parseBoolean(controlPointNode.getAttribute("active"));
        //expect only one tree pattern
        KahinaSimplePropertySensor treePatternSensor = new KahinaSimplePropertySensor(newControlPoint, stepTree);
        treePatternSensor.setPattern(KahinaSimpleProperty.importXML((Element) controlPointNode.getElementsByTagName("kahina:pattern").item(0)));
        newControlPoint.setSensor(treePatternSensor);
        return newControlPoint;
    }
}
