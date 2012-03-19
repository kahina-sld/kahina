package org.kahina.core.data.project;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.breakpoint.KahinaBreakpointProfile;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.parse.data.project.TestSet;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class KahinaProject extends KahinaObject
{
    //the Kahina application this project is assigned to, default "unkown"
    protected String appID;
    
    //for source files and the like
	private List<File> openedFiles;
	
	//TODO: move this into kahina.parse.data.project.ParseProject
	private TestSet testSet;
	
	private KahinaPerspective perspective;
	private KahinaBreakpointProfile breakpoints;
	
	public KahinaProject()
	{
	    appID = "unknown";
		openedFiles = new ArrayList<File>();
		testSet = new TestSet();
	}

	public TestSet getTestSet() 
	{
		return testSet;
	}

	public void setTestSet(TestSet testSet) 
	{
		this.testSet = testSet;
	}

	public void setPerspective(KahinaPerspective perspective) 
	{
		this.perspective = perspective;
	}

	public KahinaPerspective getPerspective() 
	{
		return perspective;
	}

	public KahinaBreakpointProfile getBreakpoints() 
	{
		return breakpoints;
	}

	public void setBreakpoints(KahinaBreakpointProfile breakpoints) 
	{
		this.breakpoints = breakpoints;
	}
	
	public Element exportXML(Document dom)
	{
        Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:project");
        el.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:appid", appID);
        Element filesEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:files");
        for (File file : openedFiles)
        {
            Element fileEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:file");
            try
            {
                fileEl.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:path", file.getCanonicalPath());
            }
            catch (DOMException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (IOException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            filesEl.appendChild(fileEl);
        }
        el.appendChild(filesEl);
        el.appendChild(perspective.exportXML(dom));
        el.appendChild(breakpoints.exportXML(dom));
        return el;
	}
	
	//TODO
	public static KahinaProject importXML(Element topEl)
	{
	    return new KahinaProject();
	}
}
