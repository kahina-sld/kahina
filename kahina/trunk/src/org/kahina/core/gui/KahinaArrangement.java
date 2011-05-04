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
	//window parameters are indexed by integer IDs
	Map<Integer,Integer> xPos;
	Map<Integer,Integer> yPos;
	Map<Integer,Integer> height;
	Map<Integer,Integer> width;
	
	//TODO: model encapsulation of views into different window types
	
	public KahinaArrangement()
	{
		xPos = new HashMap<Integer,Integer>();
		yPos = new HashMap<Integer,Integer>();
		height = new HashMap<Integer,Integer>();
		width = new HashMap<Integer,Integer>();
	}
	
	public KahinaArrangement(Map<KahinaView<?>,KahinaWindow> views)
	{
		xPos = new HashMap<Integer,Integer>();
		yPos = new HashMap<Integer,Integer>();
		height = new HashMap<Integer,Integer>();
		width = new HashMap<Integer,Integer>();
		
        int width = 300; // formerly gui.getControlPanel().getWidth();
        int height = 100;
        
        int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
        int xPos = 0;
        int yPos = 0;
        int maxY = height;
        
        //create default arrangement for all the registered views
        for (KahinaView<?> view : views.keySet())
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
            setXPos(views.get(view).getID(),xPos);
            setYPos(views.get(view).getID(),yPos);
            setWidth(views.get(view).getID(),width);
            setHeight(views.get(view).getID(),height);
        }
	}
	
	public void setXPos(int windowID, int pos)
	{
		xPos.put(windowID,pos);
	}
	
	public void setYPos(int windowID, int pos)
	{
		yPos.put(windowID,pos);
	}
	
	public void setHeight(int windowID, int pos)
	{
		height.put(windowID,pos);
	}
	
	public void setWidth(int windowID, int pos)
	{
		width.put(windowID,pos);
	}
	
	public int getXPos(int windowID)
	{
		return xPos.get(windowID);
	}
	
	public int getYPos(int windowID)
	{
		return yPos.get(windowID);
	}
	
	public int getHeight(int windowID)
	{
		return height.get(windowID);
	}
	
	public int getWidth(int windowID)
	{
		return width.get(windowID);
	}
	
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:arrangement");
		for (int windowID : xPos.keySet())
		{
			Element windowEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:window");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:id", windowID + "");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:type", "default");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:type", "title");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:xpos", xPos.get(windowID) + "");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:ypos", yPos.get(windowID) + "");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:height", height.get(windowID) + "");
			windowEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:width", width.get(windowID) + "");
			el.appendChild(windowEl);
		}
        return el;
	}
}
