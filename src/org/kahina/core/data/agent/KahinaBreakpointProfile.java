package org.kahina.core.data.agent;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.kahina.core.data.KahinaObject;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class KahinaBreakpointProfile extends KahinaObject
{
	List<KahinaBreakpoint> treeBreakpoints;
	
	public KahinaBreakpointProfile()
	{
		treeBreakpoints = new ArrayList<KahinaBreakpoint>();
	}
	
    public Element exportXML(Document dom)
    {
        Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:breakpoint-profile");
        for (KahinaBreakpoint breakpoint : treeBreakpoints)
        {
            el.appendChild(breakpoint.exportXML(dom));
        }
        return el;
    }
}
