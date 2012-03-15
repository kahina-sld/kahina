package org.kahina.qtype.data.project;

import java.io.File;

import org.kahina.core.data.project.Project;

public class QTypeProject extends Project
{
	private File grammarFile;

	public void setGrammarFile(File grammarFile) 
	{
		this.grammarFile = grammarFile;
	}

	public File getGrammarFile() 
	{
		return grammarFile;
	}
}
