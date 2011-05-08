package org.kahina.core.gui;

import java.awt.Toolkit;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.data.KahinaObject;
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
	KahinaArrangement arr;

	
	public KahinaPerspective(String appID, String name)
	{
		this.appID = appID;
		this.name = name;
		config = new HashMap<Integer,KahinaViewConfiguration>();
		visible = new HashMap<Integer,Boolean>();
		arr = new KahinaArrangement();
	}
	
	public static KahinaPerspective generateDefaultPerspective(Map<String, KahinaView<? extends KahinaObject>> nameToView)
	{
		KahinaPerspective psp = new KahinaPerspective("default", "Default");
		
		//generate windowIDs by hand; somewhat risky because the window ID system becomes easy to break
		int winID = 0;
		
        //create default arrangement for all the registered views
        for (String name : nameToView.keySet())
        {
        	psp.arr.bindNameToWindow(name,winID);
        	KahinaView<?> view = nameToView.get(name);
        	
            int width = 300; // formerly gui.getControlPanel().getWidth();
            int height = 100;
            
            int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
            int xPos = 0;
            int yPos = 0;
            int maxY = height;
            
            xPos += width + 20;
            width = view.getTitle().length() * 12 + 50;
            if (xPos + width > screenWidth)
            {
                xPos = 0;
                yPos = maxY + 20;
                maxY = 0;
            }
            height = view.getTitle().length() * 24;       
            if (height > maxY)
            {
                maxY = height;
            }
            psp.arr.setXPos(winID,xPos);
            psp.arr.setYPos(winID,yPos);
            psp.arr.setWidth(winID,width);
            psp.arr.setHeight(winID,height);
            winID++;
        }
		return psp;
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
        el.appendChild(arr.exportXML(dom));
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
		return arr;
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
