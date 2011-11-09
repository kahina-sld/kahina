package org.kahina.tralesld.data;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Set;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.gui.KahinaArrangement;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.visual.KahinaViewConfiguration;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

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
	
	/*public static KahinaPerspective importXML(Element topEl)
	{
		String appID = XMLUtilities.attrStrVal(topEl, "kahina:appID");
		String name = XMLUtilities.attrStrVal(topEl, "kahina:name");
		KahinaPerspective perspective = new KahinaPerspective(appID, name);
		NodeList nl = topEl.getElementsByTagName("kahina:arrangement");
		perspective.arr = KahinaArrangement.importXML((Element) nl.item(0));
		nl = topEl.getElementsByTagName("kahina:configuration");
		Element configEl;
		//simply process all the configurations
		for (int i = 0; i < nl.getLength(); i++)
		{
			configEl = (Element) nl.item(i);
			int viewID = XMLUtilities.attrIntVal(configEl, "kahina:viewid");
			String type = XMLUtilities.attrStrVal(configEl, "kahina:type");
			try
			{
				Method importMethod = Class.forName(type).getMethod("importXML", Element.class);
				KahinaViewConfiguration viewConfig = (KahinaViewConfiguration) importMethod.invoke(null, configEl);
				perspective.setConfiguration(viewID, viewConfig);
			}
			catch (ClassNotFoundException e)
			{
				System.err.println("ERROR in loading configuration: could not find class " + type);
			}
			catch (NoSuchMethodException e)
			{
				System.err.println("ERROR in loading configuration: class " + type + " does not implement importXML(Element e).");
			}
			catch (InvocationTargetException e)
			{
				System.err.println("InvocationTargetException while loading configuration for " + type);
			}
			catch (IllegalAccessException e)
			{
				System.err.println("IllegalAccessException while loading configuration for " + type);
			}
		}
		return perspective;
	}*/
	
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/tralesld","tralesld:workbench");
		el.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:appid", "tralesld");
		Element grammarEl = dom.createElementNS("http://www.kahina.org/xml/tralesld","tralesld:grammar");
		if (signatureFile != null)
		{
			Element signatureFileEl = dom.createElementNS("http://www.kahina.org/xml/tralesld","tralesld:file");
			signatureFileEl.setAttributeNS("http://www.kahina.org/xml/tralesld", "tralesld:filetype", "signature");
			signatureFileEl.setAttributeNS("http://www.kahina.org/xml/tralesld", "tralesld:path", signatureFile);
			grammarEl.appendChild(signatureFileEl);
			if (theoryFile != null)
			{
				Element theoryFileEl = dom.createElementNS("http://www.kahina.org/xml/tralesld","tralesld:file");
				theoryFileEl.setAttributeNS("http://www.kahina.org/xml/tralesld", "tralesld:filetype", "theory");
				theoryFileEl.setAttributeNS("http://www.kahina.org/xml/tralesld", "tralesld:path", theoryFile);
				grammarEl.appendChild(theoryFileEl);
			}
		}
		el.appendChild(grammarEl);
        Element fssEl = dom.createElementNS("http://www.kahina.org/xml/tralesld","tralesld:fss");
        for (String name : getNames())
        {
        	Element fsEl = dom.createElementNS("http://www.kahina.org/xml/tralesld","tralesld:fs");
			fsEl.setAttributeNS("http://www.kahina.org/xml/tralesld", "tralesld:name", name);
			fsEl.setAttributeNS("http://www.kahina.org/xml/tralesld", "tralesld:grisu", getStructure(name));
        	fssEl.appendChild(fsEl);
        }
        el.appendChild(fssEl);
        return el;
	}
}
