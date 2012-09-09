package org.kahina.core;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaGUI;
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
        return new KahinaProject("default");
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
    
    public void loadProject(File projectFile)
    {
        Document dom;
        try
        {
            dom = XMLUtil.parseXMLStream(new FileInputStream(projectFile), false);
            project = KahinaProject.importXML(dom.getDocumentElement());
            setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
