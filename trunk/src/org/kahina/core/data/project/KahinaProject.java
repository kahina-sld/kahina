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
import org.w3c.dom.NodeList;

public class KahinaProject extends KahinaObject
{
    //the Kahina application this project is assigned to, default "unkown"
    protected String appID;
    
    //a primary source file
    protected File mainFile;
    
    //for additional source files and the like
	protected List<File> openedFiles;
	
	protected KahinaPerspective perspective;
	
	public KahinaProject(String appID)
	{
	    this.appID = appID;
		openedFiles = new ArrayList<File>();
        this.perspective = new KahinaPerspective(appID, "default");
	}
    
    public File getMainFile()
    {
        return mainFile;
    }
    
    public void setMainFile(File mainFile)
    {
        this.mainFile = mainFile;
    }
    
    public void addOpenedFile(File file)
    {
        openedFiles.add(file);
    }
    
    public List<File> getOpenedFiles()
    {
        return openedFiles;
    }

	public void setPerspective(KahinaPerspective perspective) 
	{
		this.perspective = perspective;
	}

	public KahinaPerspective getPerspective() 
	{
		return perspective;
	}
	
	public Element exportXML(Document dom)
	{
        Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:project");
        el.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:appid", appID);
        Element mainFileEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:mainFile");
        try
        {
            mainFileEl.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:path", mainFile.getCanonicalPath());
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
        el.appendChild(mainFileEl);
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
        return el;
	}
	
	public static KahinaProject importXML(Element topEl, KahinaProject project)
	{
        if (!"kahina:project".equals(topEl.getNodeName()))
        {
            System.err.println("ERROR: attempted to loaded project file with wrong top element! Loading an empty project.");
            return project;
        }
        String appID = topEl.getAttribute("kahina:appid");
	    project.appID = appID;
        NodeList mainFileList = topEl.getElementsByTagName("kahina:mainFile");
        if (mainFileList.getLength() != 1)
        {
            System.err.println("ERROR: project file does not contain exactly one main file! Loading an empty project.");
            return project;
        }
        project.setMainFile(new File(((Element) mainFileList.item(0)).getAttribute("kahina:path")));
        NodeList fileList = topEl.getElementsByTagName("kahina:file");
        for (int i = 0; i < fileList.getLength(); i++)
        {
            project.addOpenedFile(new File(((Element) fileList.item(i)).getAttribute("kahina:path")));
        }
        NodeList perspectiveList = topEl.getElementsByTagName("kahina:perspective");
        if (mainFileList.getLength() == 0)
        {
            System.err.println("ERROR: project file does not contain a perspective declaration! Loading an empty perspective.");
            project.setPerspective(new KahinaPerspective("default","default"));
            return project;
        }
        project.setPerspective(KahinaPerspective.importXML((Element) perspectiveList.item(0)));
        return project;
	}
}
