package org.kahina.core.data.project;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.breakpoint.KahinaBreakpointProfile;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.parse.data.project.TestSet;

public class Project extends KahinaObject
{
	private List<File> openedFiles;
	private TestSet testSet;
	private KahinaPerspective perspective;
	private KahinaBreakpointProfile breakpoints;
	
	public Project()
	{
		openedFiles = new ArrayList<File>();
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

	public KahinaBreakpointProfile getBreakpoints() 
	{
		return breakpoints;
	}

	public void setBreakpoints(KahinaBreakpointProfile breakpoints) 
	{
		this.breakpoints = breakpoints;
	}
	
	
}
