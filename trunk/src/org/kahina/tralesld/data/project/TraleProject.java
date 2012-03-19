package org.kahina.tralesld.data.project;

import java.io.File;
import java.util.List;

import org.kahina.core.data.project.KahinaProject;

public class TraleProject extends KahinaProject
{
	private File signatureFile;
	private List<File> theoryFiles;
	
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
}
