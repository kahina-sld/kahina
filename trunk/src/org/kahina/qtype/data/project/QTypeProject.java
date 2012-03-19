package org.kahina.qtype.data.project;

import java.io.File;

import org.kahina.core.data.project.KahinaProject;

public class QTypeProject extends KahinaProject
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
