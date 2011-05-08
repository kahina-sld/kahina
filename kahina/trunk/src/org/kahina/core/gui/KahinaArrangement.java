package org.kahina.core.gui;

import java.awt.Toolkit;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.kahina.core.visual.KahinaView;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Storage for window configurations in a perspective.
 * <p>
 * This class represents the arrangement, size and position of all the windows in a Kahina perspective. 
 * It can be used as an instruction package for a KahinaWindowManager how to arrange and combine view windows.
 * Manipulating this does NOT directly affect window configuration, an arrangement needs to be processed by the KahinaWindowManager
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
	Map<Integer,String> title;
	
	//this mapping provides the connection between data and view windows
	
	//all the containment information is stored here, window operations manipulate this
    private HashMap<Integer,Integer> embeddingWindow;
    //TODO: type information will have to be represented if the window manager is to build up the GUI from this
    
    //TODO: decide whether these two make sense, as this information can be inferred bottom-up from embeddingWindow
    HashSet<Integer> topLevelWindows;
    HashMap<Integer,List<Integer>> childWindows;
	
	//TODO: model encapsulation of views into different window types
	//! pending design decision on encoding of arrangements
	//	1) conceptually clean: turn this into an independent representation 
	//		+ could lead to an arrangement as a primary object to generate the GUI from
	//		+ would possible make parts of KahinaGUI superfluous
	//		- very expensive and possibly redundant representation of containment hierarchy
	//		? would this representation be useful for any other purpose
	//	2) quick to implement: remove this representation, let KahinaWindowManager do this directly
	//		+ direct manipulations are quick, no spurious maintenance of identical information
	//		- much workload on the KahinaWindowManager, unclear how initialization would work
	//	3) a hybrid approach: window data here, containment hierarchy in the windows
	//		+ quickly implemented and probably easy to maintain
	//		- conceptually very unclean, Arrangement would degrade to an external copy of basic data
	
	public KahinaArrangement()
	{
		xPos = new HashMap<Integer,Integer>();
		yPos = new HashMap<Integer,Integer>();
		height = new HashMap<Integer,Integer>();
		width = new HashMap<Integer,Integer>();
		title = new HashMap<Integer,String>();
		topLevelWindows = new HashSet<Integer>();
		embeddingWindow = new HashMap<Integer,Integer>();
	}
	
	public KahinaArrangement(Map<KahinaView<?>,KahinaWindow> views)
	{
		xPos = new HashMap<Integer,Integer>();
		yPos = new HashMap<Integer,Integer>();
		height = new HashMap<Integer,Integer>();
		width = new HashMap<Integer,Integer>();
		title = new HashMap<Integer,String>();
		
		topLevelWindows = new HashSet<Integer>();
		embeddingWindow = new HashMap<Integer,Integer>();
	}
	
	public void setXPos(int windowID, int pos)
	{
		xPos.put(windowID,pos);
	}
	
	public void setYPos(int windowID, int pos)
	{
		yPos.put(windowID,pos);
	}
	
	public void setHeight(int windowID, int h)
	{
		height.put(windowID,h);
	}
	
	public void setWidth(int windowID, int w)
	{
		width.put(windowID,w);
	}
	
	public void setSize(int windowID, int w, int h)
	{
		width.put(windowID,w);
		height.put(windowID,h);
	}
	
	public void setTitle(int windowID, String t)
	{
		title.put(windowID,t);
	}
	
	public void setEmbeddingWindowID(int windowID, int embeddingID)
	{
		embeddingWindow.put(windowID, embeddingID);
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
	
	public String getTitle(int windowID)
	{
		return title.get(windowID);
	}
	
	public int getEmbeddingWindowID(int windowID)
	{
		return embeddingWindow.get(windowID);
	}
	
	/*
	 * Informal description of the XML format:
	 * (TODO: replace this by a formal specification as an XML schema or similar)
	 * - encodings of embeddings does not mirror internal structure because it can be mapped
	 *   very nicely onto an XML encoding that is much easier to edit in XML without introducing inconsistencies
	 * - binding of live views to the structures they are to represent is defined via the displayIDs
	 * - snapshot clones are represented, but neither imported nor exported because they cannot reliably be restored
	 * - TODO: offer an option to linearize and restore contents of snapshot clones as well
	 */
	
	
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
