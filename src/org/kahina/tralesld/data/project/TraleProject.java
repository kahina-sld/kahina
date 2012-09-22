package org.kahina.tralesld.data.project;

import java.io.File;
import java.util.LinkedList;
import java.util.List;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.parse.data.project.TestSet;
import org.kahina.parse.data.project.TestSetExtension;
import org.w3c.dom.Element;

public class TraleProject extends LogicProgrammingProject implements TestSetExtension
{
	private File signatureFile;
	private List<File> theoryFiles;
    private TestSet testSet;
    
    public TraleProject(String name, KahinaTree stepTree, KahinaController control)
    {
        super("trale", name, stepTree, control);
        theoryFiles = new LinkedList<File>();
        testSet = new TestSet();
    }
	
	public void setSignatureFile(File signatureFile) 
	{
		this.signatureFile = signatureFile;
	}
	
	public File getSignatureFile() 
	{
		return signatureFile;
	}
	
	public void setTheoryFiles(List<File> theoryFiles) 
	{
		this.theoryFiles = theoryFiles;
	}
	
	public List<File> getTheoryFiles() 
	{
		return theoryFiles;
	}

    public TestSet getTestSet()
    {
        return testSet;
    }

    public void setTestSet(TestSet testSet)
    {
        this.testSet = testSet;
    }
    
    //TODO: import all the structures produced by exportXML
    public static TraleProject importXML(Element topEl, TraleProject project)
    {
        LogicProgrammingProject.importXML(topEl, project);
        return project;
    }
}
