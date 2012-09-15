package org.kahina.tulipa;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.io.util.XMLUtil;
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
        return new KahinaProject("tulipa");
    }
    
    public void loadProject(File projectFile)
    {
        Document dom;
        try
        {
            dom = XMLUtil.parseXMLStream(new FileInputStream(projectFile), false);
            project = createNewProject();
            project = KahinaProject.importXML(dom.getDocumentElement(), project);
            setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
