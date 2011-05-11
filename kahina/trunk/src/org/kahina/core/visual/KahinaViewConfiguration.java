package org.kahina.core.visual;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * The ancestor class of all view configuration classes.
 * <p>
 * Implementations of this class serve as stores for those
 * properties of a view component that can be manipulated by the user.
 * Implementations of this class should contain all the data necessary
 * to reconstruct a view exactly as it was when restoring a session.
 * The default implementation contains no properties
 * <p>
 * This class is usually specialized for any given subclass of {@link KahinaView}.
 * @author jdellert
 *
 */
public class KahinaViewConfiguration
{	
	/**
	 * Default implementation creates an empty KahinaViewConfiguration
	 * @param e the DOM node the configuration is to be read from
	 * @return the configuration encoded in the DOM node
	 */
	public static KahinaViewConfiguration importXML(Element e)
	{
		return new KahinaViewConfiguration();
	}
	
	/**
	 * Default implementation creates an empty element without type specification
	 */
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:configuration");
		el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:type","org.kahina.core.visual.chart.KahinaViewConfiguration");
		return el;
	}
}
