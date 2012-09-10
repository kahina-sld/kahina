package org.kahina.qtype.data.project;

import java.io.File;

import org.kahina.core.data.breakpoint.KahinaControlPointProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.lp.data.project.LpControlAgentExtension;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class QTypeProject extends KahinaProject implements LpControlAgentExtension
{ 
    /**
     * 
     */
    private static final long serialVersionUID = -2093492979848559602L;

    public QTypeProject()
    {
        super("qtype");
    }
    
    public static QTypeProject importXML(Element topEl)
    {
        if (!"kahina:project".equals(topEl.getNodeName()))
        {
            System.err.println("ERROR: attempted to loaded project file with wrong top element! Loading an empty project.");
            return new QTypeProject();
        }
        String appID = topEl.getAttribute("kahina:appid");
        if (!"qtype".equals(appID))
        {
            System.err.println("ERROR: attempted to loaded project file for different application " + appID + "! Loading an empty project instead.");
            return new QTypeProject();
        }
        QTypeProject project = new QTypeProject();
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
