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
		return null;
	}

	@Override
	protected void createTreeBehavior() 
	{
		// TODO Auto-generated method stub	
	}
    
    public KahinaProject loadProject(File projectFile)
    {
        Document dom;
        KahinaProject project = new KahinaProject("default", "no name");
        try
        {
            dom = XMLUtil.parseXMLStream(new FileInputStream(projectFile), false);      
            project = KahinaProject.importXML(dom.getDocumentElement(), project);
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return project;
    }

    @Override
    protected void prepareProjectLists()
    {
        recentProjects = new LinkedList<KahinaProject>();
        // load the default perspectives in the bin folder of the respective Kahina application
        defaultProjects = new LinkedList<KahinaProject>();
        // This filter only returns XML files
        FileFilter fileFilter = new FileFilter()
        {
            public boolean accept(File file)
            {
                // System.err.println("Filtering file " + file.getName() + ": "
                // + file.getName().endsWith("xml"));
                return file.getName().endsWith("xml");
            }
        };
        File[] files = new File(this.getClass().getResource("./data/project").getFile()).listFiles(fileFilter);
        for (File f : files)
        {
            if (VERBOSE)
            {
                System.err.println("Loading predefined project: " + f.getAbsolutePath());
            }
            defaultProjects.add(loadProject(f));
        }
        
    } 
}
