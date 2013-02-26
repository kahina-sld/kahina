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
import org.kahina.tralesld.TraleSLDInstance;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class TraleProject extends LogicProgrammingProject implements TestSetExtension
{
	private File signatureFile;
	private List<File> theoryFiles;
    private TestSet testSet;
    
    public TraleProject(String name, KahinaTree stepTree, TraleSLDInstance kahina)
    {
        super("trale", name, stepTree, kahina);
        theoryFiles = new LinkedList<File>();
        testSet = new TestSet();
    }
    
    public TraleProject copy()
    {
        TraleProject copy = new TraleProject(new String(name), stepTree, (TraleSLDInstance) kahina);
        copyDataInto(copy);
        return copy;
    }
   
    public void copyDataInto(TraleProject copy)
    {
        super.copyDataInto(copy);
        copy.signatureFile = new File(signatureFile.getAbsolutePath());
        copy.theoryFiles.clear();
        for (File theoryFile : theoryFiles)
        {
            copy.theoryFiles.add(new File(theoryFile.getAbsolutePath()));
        }
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
    
    public static void importXML(Element topEl, TraleProject project)
    {
        LogicProgrammingProject.importXML(topEl, project);
        NodeList mainFileList = topEl.getElementsByTagName("trale:signatureFile");
        if (mainFileList.getLength() != 1)
        {
            System.err.println("WARNING: project does not define exactly one signature file!");
        }
        else
        {
            File signatureFile = new File(((Element) mainFileList.item(0)).getAttribute("kahina:path"));
            project.setMainFile(signatureFile);
            project.setSignatureFile(signatureFile);
        }
        NodeList fileList = topEl.getElementsByTagName("trale:theoryFile");
        for (int i = 0; i < fileList.getLength(); i++)
        {
            File theoryFile = new File(((Element) mainFileList.item(0)).getAttribute("kahina:path"));
            project.addOpenedFile(theoryFile);
            project.getTheoryFiles().add(theoryFile);
        }
    }
}
