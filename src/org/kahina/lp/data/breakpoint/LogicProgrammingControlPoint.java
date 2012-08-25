package org.kahina.lp.data.breakpoint;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaStepPropertySensor;
import org.kahina.core.control.KahinaTreePatternSensor;
import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.patterns.TreePatternNode;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.io.color.ColorUtil;
import org.kahina.lp.LogicProgrammingState;
import org.w3c.dom.Element;

public class LogicProgrammingControlPoint extends KahinaControlPoint
{
    protected KahinaTreePatternSensor sensor;
    protected KahinaTree stepTree;
    
    public LogicProgrammingControlPoint(KahinaController control, LogicProgrammingState state)
    {
        super(control);
        this.stepTree = state.getStepTree();
        setSensor(new KahinaTreePatternSensor(this, stepTree));
    }
    
    public KahinaTree getStepTree()
    {
        return stepTree;
    }
    
    public void setStepTree(KahinaTree stepTree)
    {
        this.stepTree = stepTree;
    }
    
    public KahinaTreePatternSensor getSensor()
    {
        return sensor;
    }
    
    public void setSensor(KahinaTreePatternSensor sensor)
    {
        super.setSensor(sensor);
        this.sensor = sensor;
    }
    
    /**
     * Constructs a logic programming control point from an XML representation as produced by <code>exportXML</code>.
     * @param controlPointNode an XML DOM element with name "controlPoint" as produced when parsing the result of <code>exportXML</code>
     * @return a new logic programming control point object corresponding to the XML representation contained in the DOM element
     */
    public static LogicProgrammingControlPoint importXML(Element controlPointNode, KahinaController control, LogicProgrammingState state)
    {
        LogicProgrammingControlPoint newControlPoint = new LogicProgrammingControlPoint(control, state);
        newControlPoint.setName(controlPointNode.getAttribute("name"));
        newControlPoint.setSignalColor(ColorUtil.decodeHTML(controlPointNode.getAttribute("color")));
        newControlPoint.active = Boolean.parseBoolean(controlPointNode.getAttribute("active"));
        //expect only one tree pattern
        KahinaTreePatternSensor treePatternSensor = new KahinaTreePatternSensor(newControlPoint, state.getStepTree());
        treePatternSensor.setPattern(TreePatternNode.importXML((Element) controlPointNode.getElementsByTagName("kahina:pattern").item(0)));
        newControlPoint.setSensor(treePatternSensor);
        return newControlPoint;
    }
}
