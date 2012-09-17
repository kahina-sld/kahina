package org.kahina.qtype.data.project;

import java.io.File;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.breakpoint.KahinaControlAgentProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class QTypeProject extends LogicProgrammingProject
{ 
    /**
     * 
     */
    private static final long serialVersionUID = -2093492979848559602L;

    public QTypeProject(KahinaTree stepTree, KahinaController control)
    {
        super("qtype", stepTree, control);
    }
    
    public static QTypeProject importXML(Element topEl, QTypeProject project, KahinaController control)
    {
        LogicProgrammingProject.importXML(topEl, project, control);
        return project;
    }
}
