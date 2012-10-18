package org.kahina.core;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.LinkedList;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.io.util.XMLUtil;
import org.w3c.dom.Document;

public class KahinaDefaultInstance extends KahinaInstance<KahinaState, KahinaGUI, KahinaBridge, KahinaProject>
{
	@Override
	protected KahinaBridge createBridge() 
	{
		return new KahinaBridge(this);
	}

	@Override
	protected KahinaGUI createGUI() 
	{
		return null;
	}
    
    @Override
    protected KahinaProject createNewProject()
    {
        return new KahinaProject("default", "name");
    }

	@Override
	protected KahinaState createState() 
	{
		return new KahinaState(this);
	}

	@Override
	protected void createTreeBehavior() 
	{
		// TODO Auto-generated method stub	
	}
    
    public KahinaProject loadProject(InputStream stream)
    {
        Document dom;
        KahinaProject project = new KahinaProject("default", "no name");
        dom = XMLUtil.parseXMLStream(stream, false);      
        project = KahinaProject.importXML(dom.getDocumentElement(), project);
        return project;
    }

    @Override
    protected void prepareProjectLists()
    {
        recentProjects = new LinkedList<KahinaProject>();
        defaultProjects = new LinkedList<KahinaProject>();
    }

    @Override
    protected void preparePerspectiveLists()
    {
        recentPerspectives = new LinkedList<KahinaPerspective>();
        defaultPerspectives = new LinkedList<KahinaPerspective>();  
    } 
}
