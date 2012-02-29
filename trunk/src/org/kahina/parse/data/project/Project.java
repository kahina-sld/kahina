package org.kahina.parse.data.project;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.gui.KahinaPerspective;

public class Project extends KahinaObject
{
	private TestSet testSet;
	private KahinaPerspective perspective;
	
	public Project()
	{
		testSet = new TestSet();
	}

	public TestSet getTestSet() 
	{
		return testSet;
	}

	public void setTestSet(TestSet testSet) 
	{
		this.testSet = testSet;
	}

	public void setPerspective(KahinaPerspective perspective) 
	{
		this.perspective = perspective;
	}

	public KahinaPerspective getPerspective() 
	{
		return perspective;
	}
}
