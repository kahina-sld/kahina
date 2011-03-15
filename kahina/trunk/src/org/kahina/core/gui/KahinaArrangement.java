package org.kahina.core.gui;

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
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:arrangement");
        return el;
	}
}
