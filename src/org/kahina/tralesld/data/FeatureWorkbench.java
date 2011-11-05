package org.kahina.tralesld.data;

import java.util.HashMap;
import java.util.Set;

import org.kahina.core.data.KahinaObject;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.signature.TraleSLDSignature;

public class FeatureWorkbench extends KahinaObject
{
	//the feature structures on the workbench, accessible via string IDs for now
	private HashMap<String, String> obj;
	
	private String signatureFile = null;
	private String theoryFile = null;
	
	private TraleSLDSignature signature;
	
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
	
	public String removeStructure(String id)
	{
		return obj.remove(id);
	}

	public Set<String> getNames() 
	{
		return obj.keySet();
	}

	public String getSignatureFileName() 
	{
		return signatureFile;
	}

	public void setSignatureFileName(String signatureFile) 
	{
		this.signatureFile = signatureFile;
	}

	public String getTheoryFileName() 
	{
		return theoryFile;
	}

	public void setTheoryFileName(String theoryFile) 
	{
		this.theoryFile = theoryFile;
	}
	
	public TraleSLDSignature getSignature()
	{
		return signature;
	}
	
	public void setSignature(TraleSLDSignature signature)
	{
		this.signature = signature;
	}
}
