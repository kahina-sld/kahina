package org.kahina.tralesld.data;

import java.util.HashMap;
import java.util.Set;

import org.kahina.core.data.KahinaObject;
import org.kahina.tralesld.data.fs.TraleSLDFS;

public class FeatureWorkbench extends KahinaObject
{
	//the feature structures on the workbench, accessible via string IDs for now
	private HashMap<String, String> obj;
	
	public FeatureWorkbench()
	{
		obj = new HashMap<String, String>();
	}
	
	public void storeStructure(String id, String grisuString)
	{
		obj.put(id, grisuString);
	}
	
	public String getStructure(String id)
	{
		return obj.get(id);
	}

	public Set<String> getNames() 
	{
		return obj.keySet();
	}
}
