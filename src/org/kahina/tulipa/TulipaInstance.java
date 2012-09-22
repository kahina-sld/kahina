package org.kahina.tulipa;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.LinkedList;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.tulipa.behavior.TulipaDAGBehavior;
import org.kahina.tulipa.bridge.TulipaBridge;
import org.kahina.tulipa.data.grammar.TulipaGrammar;
import org.kahina.tulipa.gui.TulipaGUI;
import org.kahina.tulipa.visual.grammar.TulipaGrammarView;
import org.w3c.dom.Document;

public class TulipaInstance extends KahinaInstance<TulipaState, TulipaGUI, TulipaBridge, KahinaProject>
{
    
    @Override
    protected void createTreeBehavior()
    {
        new TulipaDAGBehavior(state.getDAG(), this);
    }

    @Override
    protected TulipaBridge createBridge()
    {
        return new TulipaBridge(this);
    }

    @Override
    protected TulipaGUI createGUI()
    {
        return new TulipaGUI(TulipaStep.class, this);
    }

    @Override
    protected TulipaState createState()
    {
    	return new TulipaState();
    }

    @Override
    public TulipaState getState()
    {
        return state;
    }

    @Override
	protected void fillViewRegistry()
	{
		super.fillViewRegistry();
		KahinaViewRegistry.registerMapping(TulipaGrammar.class, TulipaGrammarView.class);
	}

    @Override
    protected KahinaProject createNewProject()
    {
        return new KahinaProject("tulipa", "no name");
    }
    
    public KahinaProject loadProject(File projectFile)
    {
        Document dom;
        KahinaProject project = createNewProject();
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
        File[] files = new File(KahinaInstance.class.getResource("./data/project").getFile()).listFiles(fileFilter);
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
