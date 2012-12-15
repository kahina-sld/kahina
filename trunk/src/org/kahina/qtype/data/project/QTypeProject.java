package org.kahina.qtype.data.project;

import java.io.File;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.agent.KahinaControlAgentProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.qtype.QTypeDebuggerInstance;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class QTypeProject extends LogicProgrammingProject
{ 
    /**
     * 
     */
    private static final long serialVersionUID = -2093492979848559602L;

    public QTypeProject(String name, KahinaTree stepTree, QTypeDebuggerInstance kahina)
    {
        super("qtype", name, stepTree, kahina);
    }
    
    public static QTypeProject importXML(Element topEl, QTypeProject project, QTypeDebuggerInstance kahina, KahinaTree stepTree)
    {
        LogicProgrammingProject.importXML(topEl, project, kahina, stepTree);
        return project;
    }
    
    public QTypeProject copy()
    {
        QTypeProject copy = new QTypeProject(new String(name), stepTree, (QTypeDebuggerInstance) kahina);
        copyDataInto(copy);
        return copy;
    }
}
