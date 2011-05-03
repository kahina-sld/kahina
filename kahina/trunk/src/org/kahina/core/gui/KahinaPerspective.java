package org.kahina.core.gui;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.visual.KahinaView;
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
	//the Kahina application this perspective is assigned to
	String appID;
	//the name this perspective is referred by in the GUI
	String name;
	
	//view options are indexed by integer IDs
	Map<Integer,KahinaViewConfiguration> config;
	//visibility status of windows (and views)
	Map<Integer,Boolean> visible;
	//the arrangement, size and position of windows
	KahinaArrangement arrangement;

	
	public KahinaPerspective(String appID, String name)
	{
		this.appID = appID;
		this.name = name;
		config = new HashMap<Integer,KahinaViewConfiguration>();
		visible = new HashMap<Integer,Boolean>();
		arrangement = new KahinaArrangement();
	}
	
	public KahinaPerspective(String appID, String name,List<KahinaView<?>> views)
	{
		config = new HashMap<Integer,KahinaViewConfiguration>();
		visible = new HashMap<Integer,Boolean>();
		arrangement = new KahinaArrangement(views);
	}
	
	public static KahinaPerspective importXML(Element e)
	{
		KahinaPerspective perspective = new KahinaPerspective("default", "Default");
		return perspective;
	}
	
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:perspective");
		el.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:appid", appID);
        el.appendChild(arrangement.exportXML(dom));
        Element configsEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:configurations");
        for (int viewID : config.keySet())
        {
        	Element configEl = this.getConfiguration(viewID).exportXML(dom);
        	configEl.setAttributeNS("http://www.kahina.org/xml/kahina", "kahina:viewid", viewID + "");
        	configsEl.appendChild(configEl);
        }
        el.appendChild(configsEl);
        return el;
	}
	
	public KahinaViewConfiguration getConfiguration(int viewID)
	{
		KahinaViewConfiguration conf = config.get(viewID);
		if (conf == null) return new KahinaViewConfiguration();
		return conf;
	}
	
	public void setConfiguration(int viewID, KahinaViewConfiguration conf)
	{
		config.put(viewID, conf);
	}
	
	public String getAppID()
	{
		return appID;
	}
	
	public String getName()
	{
		return name;
	}
	
	public KahinaArrangement getArrangement()
	{
		return arrangement;
	}
	
	public void setVisibility(int viewID, boolean vis)
	{
		visible.put(viewID, vis);
	}
	
	public void toggleVisibility(int viewID)
	{
		setVisibility(viewID, !isVisible(viewID));
	}
	
	//windows and views are visible by default
	public boolean isVisible(int viewID)
	{
		Boolean vis = visible.get(viewID);
		if (vis == null) return true;
		return vis;
	}
}
