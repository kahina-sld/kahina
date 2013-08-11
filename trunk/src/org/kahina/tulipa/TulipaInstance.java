package org.kahina.tulipa;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.LinkedList;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaPerspective;
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
    	return new TulipaState(this);
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
    
    @Override
    public KahinaProject loadProject(File file)
    {
    	InputStream stream;
		try
		{
			stream = new FileInputStream(file);
		} catch (FileNotFoundException e)
		{
			System.err.println("ERROR: Project file not found!");
			e.printStackTrace();
			return null;
		}
        Document dom;
        KahinaProject project = createNewProject();
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
