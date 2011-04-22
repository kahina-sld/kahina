package org.kahina.core.gui;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.visual.KahinaViewConfiguration;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Storage of window configuration and display options for a Kahina instance.
 * <p>
 * Main functionality is to represent sets of configurations.
 * A {@link KahinaWindowManager} can build a Kahina environment or re-arrange it
 * according to the instructions contained in a perspective. 
 * <p>
 * It is the task of the {@link KahinaWindowManager} to react to user-imposed changes
 * of display options and window layout by changing the corresponding setting
 * in the current perspective.
 * <p>
 * A perspective defines one of the important parts of an
 * application profile, and is therefore usually persistent across sessions.
 * 
 * @author jdellert
 *
 */

public class KahinaPerspective 
{
	//view options are indexed by string identifiers
	Map<String,KahinaViewConfiguration> config;
	//the arrangement, size and position of windows
	KahinaArrangement arrangement;
	
	public KahinaPerspective()
	{
		config = new HashMap<String,KahinaViewConfiguration>();
		arrangement = new KahinaArrangement();
	}
	
	public static KahinaPerspective importXML(Element e)
	{
		KahinaPerspective perspective = new KahinaPerspective();
		return perspective;
	}
	
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:perspective");
        el.appendChild(arrangement.exportXML(dom));
        Element configsEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:configurations");
        for (String viewID : config.keySet())
        {
        	Element configEl = this.getConfiguration(viewID).exportXML(dom);
        	configEl.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:viewid", viewID);
        	configsEl.appendChild(configEl);
        }
        el.appendChild(configsEl);
        return el;
	}
	
	public KahinaViewConfiguration getConfiguration(String viewID)
	{
		KahinaViewConfiguration conf = config.get(viewID);
		if (conf == null) return new KahinaViewConfiguration();
		return conf;
	}
	
	public void setConfiguration(String viewID, KahinaViewConfiguration conf)
	{
		config.put(viewID, conf);
	}
}
