package org.kahina.core.gui;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.kahina.core.io.util.XMLUtilities;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Storage for window configurations in a perspective.
 * <p>
 * This class represents the arrangement, size and position of all the windows in a Kahina perspective. 
 * It can be used as an instruction package for a KahinaWindowManager how to arrange and combine view windows.
 * Manipulating this does NOT directly affect window configuration, an arrangement needs to be processed by the KahinaWindowManager.
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
	
	//this mapping provides the connection between node data and (primary) associated view windows
	//the keys of this mapping constitute the seed for bottom-up embedding tree construction
	private Map<Integer,String> winIDToBinding;
	//with each name we associate a primary window, the others are clones; "main" reserved for main window
	private Map<String,Integer> primaryWindow;
	
	//all the containment information is stored here, window operations manipulate this
    private HashMap<Integer,Integer> embeddingWindow;
    //type information is needed by the window manager to build up the GUI from this
    private HashMap<Integer,Integer> windowType;
    
    //TODO: decide whether these two make sense, as this information can be inferred bottom-up from embeddingWindow
    //will not use these for import and export for now
    HashSet<Integer> topLevelWindows;
    HashMap<Integer,List<Integer>> childWindows;
	
	public KahinaArrangement()
	{
		xPos = new HashMap<Integer,Integer>();
		yPos = new HashMap<Integer,Integer>();
		height = new HashMap<Integer,Integer>();
		width = new HashMap<Integer,Integer>();
		title = new HashMap<Integer,String>();
		winIDToBinding = new HashMap<Integer,String>();
		primaryWindow = new HashMap<String,Integer>();
		windowType = new HashMap<Integer,Integer>();
		
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
	
	/**
	 * Sets the window type for some windowID.
	 * Refuses to set the type for some window where it is already defined.
	 * @param windowID
	 * @param type
	 */
	public void setWindowType(int windowID, int type)
	{
		if (windowType.get(windowID) == null)
		{
			windowType.put(windowID, type);
		}
		else
		{
			System.err.println("WARNING: Cannot change type of window " + windowID + " to " + type + "!");
		}
	}
	
	public void setEmbeddingWindowID(int windowID, int embeddingID)
	{
		embeddingWindow.put(windowID, embeddingID);
	}
	
	public void setPrimaryWindow(String binding, int winID)
	{
		primaryWindow.put(binding,winID);
		bindWindow(winID,binding);
	}
	
	public void bindWindow(int windowID, String binding)
	{
		winIDToBinding.put(windowID,binding);
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
	
	public int getWindowType(int windowID)
	{
		return windowType.get(windowID);
	}
	
	public int getEmbeddingWindowID(int windowID)
	{
		return embeddingWindow.get(windowID);
	}
	
	public String getBindingForWinID(int winID)
	{
		return winIDToBinding.get(winID);
	}
	
	public int getPrimaryWinIDForName(String name)
	{
		return primaryWindow.get(name);
	}
	
	/*
	 * Informal description of the XML format:
	 * (TODO: replace this by a formal specification as an XML schema or similar)
	 * - encodings of embeddings does not mirror internal storage format because it can be mapped
	 *   very nicely onto a tree structure that is much easier to edit in XML without introducing inconsistencies
	 * - binding of live views to the structures they are to represent is defined via the displayIDs
	 * - snapshot clones are represented, but neither imported nor exported because they cannot reliably be restored
	 * - TODO: offer an option to linearize and restore contents of snapshot clones as well
	 */
	
	public static KahinaArrangement importXML(Element topEl)
	{
		KahinaArrangement arr = new KahinaArrangement();
		NodeList nl = topEl.getElementsByTagName("kahina:default-window");
		Element el;
		//start at the leaves of the embedding hierarchy and work bottom-up
		for (int i = 0; i < nl.getLength(); i++)
		{
			el = (Element) nl.item(i);
			int previousID = -1;
			int winID = -1;
			while (el != topEl)
			{
				winID = XMLUtilities.attrIntVal(el, "kahina:id");
				if (previousID != -1)
				{
					arr.embeddingWindow.put(previousID,winID);
				}
				System.err.println("Now loading winID " + winID);
				arr.setXPos(winID, XMLUtilities.attrIntVal(el, "kahina:xpos"));
				arr.setYPos(winID, XMLUtilities.attrIntVal(el, "kahina:ypos"));
				arr.setWidth(winID, XMLUtilities.attrIntVal(el, "kahina:width"));
				arr.setHeight(winID, XMLUtilities.attrIntVal(el, "kahina:height"));
				arr.setTitle(winID, XMLUtilities.attrStrVal(el, "kahina:title"));
				String type = el.getLocalName();
				if (type.equals("default-window"))
				{
					arr.setWindowType(winID, KahinaWindowType.DEFAULT_WINDOW);
					String binding = XMLUtilities.attrStrVal(el, "kahina:binding");
					arr.bindWindow(winID, binding);
					if (XMLUtilities.attrBoolVal(el, "kahina:primary"))
					{
						arr.setPrimaryWindow(binding, winID);
					}
				}
				else if (type.equals("hori-split-window"))
				{
					arr.setWindowType(winID, KahinaWindowType.HORI_SPLIT_WINDOW);
				}
				else if (type.equals("veri-split-window"))
				{
					arr.setWindowType(winID, KahinaWindowType.VERT_SPLIT_WINDOW);
				}
				else if (type.equals("tabbed-window"))
				{
					arr.setWindowType(winID, KahinaWindowType.TABBED_WINDOW);
				}
				previousID = winID;
				el = (Element) el.getParentNode();
			}
		}
		return arr;
	}
	
	//TODO: somehow get the order of the elements right! problem is that content windows can come in in any order!
	public Element exportXML(Document dom)
	{
		Element topEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:arrangement");
		HashMap<Integer,Element> constructedNodes = new HashMap<Integer,Element>();
		for (Integer windowID : winIDToBinding.keySet())
		{
			//System.err.println("Processing windowID " + windowID);
			Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:default-window");
			el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:id", windowID + "");
			el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:primary", (primaryWindow.get(winIDToBinding.get(windowID)) == windowID) + "");
			el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:binding", winIDToBinding.get(windowID));
			el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:title", title.get(windowID));
			el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:xpos", xPos.get(windowID) + "");
			el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:ypos", yPos.get(windowID) + "");
			el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:height", height.get(windowID) + "");
			el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:width", width.get(windowID) + "");
			constructedNodes.put(windowID, el);
			//potentially confusing, but sound and efficient: reuse windowID and el to march up the embedding hierarchy
			while (windowID != null && windowID != -1)
			{
				windowID = embeddingWindow.get(windowID);
				//System.err.println("	Recursion into windowID " + windowID);
				if (windowID == null || windowID == -1)
				{
					topEl.appendChild(el);
				}
				else if (constructedNodes.get(windowID) != null)
				{
					constructedNodes.get(windowID).appendChild(el);
					break;
				}
				else
				{
					System.err.println("Window type: " + windowType.get(windowID));
					switch (windowType.get(windowID))
					{
						case KahinaWindowType.HORI_SPLIT_WINDOW:
						{
							Element embeddingEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:hori-split-window");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:id", windowID + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:title", title.get(windowID));
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:xpos", xPos.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:ypos", yPos.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:height", height.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:width", width.get(windowID) + "");
							embeddingEl.appendChild(el);
							constructedNodes.put(windowID, embeddingEl);
							el = embeddingEl;
							break;
						}
						case KahinaWindowType.VERT_SPLIT_WINDOW:
						{
							Element embeddingEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:vert-split-window");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:id", windowID + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:title", title.get(windowID));
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:xpos", xPos.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:ypos", yPos.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:height", height.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:width", width.get(windowID) + "");
							embeddingEl.appendChild(el);
							constructedNodes.put(windowID, embeddingEl);
							el = embeddingEl;
							break;
						}
						case KahinaWindowType.TABBED_WINDOW:
						{
							Element embeddingEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:tabbed-window");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:id", windowID + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:title", title.get(windowID));
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:xpos", xPos.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:ypos", yPos.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:height", height.get(windowID) + "");
							embeddingEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:width", width.get(windowID) + "");
							embeddingEl.appendChild(el);
							constructedNodes.put(windowID, embeddingEl);
							el = embeddingEl;
							break;
						}
					}
				}
			}
		}
        return topEl;
	}
}
