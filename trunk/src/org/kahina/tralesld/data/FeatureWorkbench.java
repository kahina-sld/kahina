package org.kahina.tralesld.data;

import java.util.HashMap;

import org.kahina.core.data.KahinaObject;
import org.kahina.tralesld.data.fs.TraleSLDFS;

public class FeatureWorkbench extends KahinaObject
{
	//the feature structures on the workbench, accessible via string IDs for now
	HashMap<String, TraleSLDFS> obj;
	
	public FeatureWorkbench()
	{
		obj = new HashMap<String, TraleSLDFS>();
	}
	
	public void storeStructure(String id, TraleSLDFS structure)
	{
		obj.put(id, structure);
	}
	
	public TraleSLDFS getStructure(String id)
	{
		return obj.get(id);
	}
}
