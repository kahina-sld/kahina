package org.kahina.tralesld.data.project;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.kahina.core.data.tree.KahinaTree;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.parse.data.project.TestSet;
import org.kahina.parse.data.project.TestSetExtension;
import org.kahina.tralesld.TraleSLDInstance;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class TraleProject extends LogicProgrammingProject implements TestSetExtension
{

	private static final long serialVersionUID = 194694387410246329L;
	
	private File signatureFile;
	private List<File> theoryFiles;
    private TestSet testSet;
    
    public TraleProject(String name, TraleSLDInstance kahina)
    {
        super("trale", name,kahina);
        theoryFiles = new LinkedList<File>();
        testSet = new TestSet();
    }
    
    public TraleProject copy()
    {
        TraleProject copy = new TraleProject(new String(name), (TraleSLDInstance) kahina);
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
        copy.setTestSet(testSet.copy());
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
    
    public static void importXML(Element topEl, TraleProject project, TraleSLDInstance kahina, KahinaTree stepTree)
    {
        LogicProgrammingProject.importXML(topEl, project, kahina, stepTree);
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
            File theoryFile = new File(((Element) fileList.item(i)).getAttribute("kahina:path"));
            project.addOpenedFile(theoryFile);
            project.getTheoryFiles().add(theoryFile);
        }
        NodeList testSet = topEl.getElementsByTagName("kahina:testItem");
        for (int i = 0; i < testSet.getLength(); i++)
        {
            String testSentence = ((Element) testSet.item(i)).getAttribute("kahina:sentence");
            project.getTestSet().addSentence(testSentence);
        }
    }
    
    public Element exportXML(Document dom)
    {
        Element el = super.exportXML(dom);
        if (signatureFile != null)
        {
            Element signatureFileEl = dom.createElementNS("http://www.kahina.org/xml/trale","trale:signatureFile");
            try
            {
                signatureFileEl.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:path", signatureFile.getCanonicalPath());
            }
            catch (DOMException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (IOException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            el.appendChild(signatureFileEl);
        }
        for (File file : theoryFiles)
        {
            Element fileEl = dom.createElementNS("http://www.kahina.org/xml/trale","trale:theoryFile");
            try
            {
                fileEl.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:path", file.getCanonicalPath());
            }
            catch (DOMException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            catch (IOException e)
            {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            el.appendChild(fileEl);
        }
        Element testSetEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:testSet");
        for (String testSentence : testSet.getSentences())
        {
            Element testItemEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:testItem");
            testItemEl.setAttribute("kahina:sentence", testSentence);
            testSetEl.appendChild(testItemEl);
        }
        el.appendChild(testSetEl);
        return el;
    }
}
