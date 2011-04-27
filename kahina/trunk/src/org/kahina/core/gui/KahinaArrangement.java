package org.kahina.core.gui;

import java.awt.Toolkit;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.KahinaViewConfiguration;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Storage for window configurations in a perspective.
 * <p>
 * This class represents the arrangement, size and position of all the windows in a Kahina perspective. 
 * It can be used as an instruction package for a KahinaWindowManager how to arrange and combine view windows.
 * <p>
 * Arrangements can be stored and restored as parts of profiles for persistence.
 * @author jdellert
 *
 */
public class KahinaArrangement 
{
	//window parameters are indexed by string identifiers
	Map<String,Integer> xPos;
	Map<String,Integer> yPos;
	Map<String,Integer> height;
	Map<String,Integer> width;
	
	//TODO: model encapsulation of views into different window types
	
	public KahinaArrangement()
	{
		xPos = new HashMap<String,Integer>();
		yPos = new HashMap<String,Integer>();
		height = new HashMap<String,Integer>();
		width = new HashMap<String,Integer>();
	}
	
	public KahinaArrangement(List<KahinaView<?>> views)
	{
		xPos = new HashMap<String,Integer>();
		yPos = new HashMap<String,Integer>();
		height = new HashMap<String,Integer>();
		width = new HashMap<String,Integer>();
		
        int width = 300; // formerly gui.getControlPanel().getWidth();
        int height = 100;
        
        int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
        int xPos = 0;
        int yPos = 0;
        int maxY = height;
        
        //create default arrangement for all the registered views
        for (KahinaView<?> view : views)
        {
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
            setXPos(view.getTitle(),xPos);
            setYPos(view.getTitle(),yPos);
            setWidth(view.getTitle(),width);
            setHeight(view.getTitle(),height);
        }
	}
	
	public void setXPos(String windowID, int pos)
	{
		xPos.put(windowID,pos);
	}
	
	public void setYPos(String windowID, int pos)
	{
		yPos.put(windowID,pos);
	}
	
	public void setHeight(String windowID, int pos)
	{
		height.put(windowID,pos);
	}
	
	public void setWidth(String windowID, int pos)
	{
		width.put(windowID,pos);
	}
	
	public int getXPos(String windowID)
	{
		return xPos.get(windowID);
	}
	
	public int getYPos(String windowID)
	{
		return yPos.get(windowID);
	}
	
	public int getHeight(String windowID)
	{
		return height.get(windowID);
	}
	
	public int getWidth(String windowID)
	{
		return width.get(windowID);
	}
	
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:arrangement");
		for (String windowID : xPos.keySet())
		{
			Element windowEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:window");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:id", windowID);
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:type", "default");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:xpos", xPos.get(windowID) + "");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:ypos", yPos.get(windowID) + "");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:height", height.get(windowID) + "");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:width", width.get(windowID) + "");
			el.appendChild(windowEl);
		}
        return el;
	}
}
