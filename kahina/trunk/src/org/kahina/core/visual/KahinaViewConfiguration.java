package org.kahina.core.visual;

import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.KahinaPerspective;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * The generic ancestor class of all view configuration classes.
 * <p>
 * Implementations of this class serve as stores for those
 * properties of a view component that can be manipulated by the user.
 * Implementations of this class should contain all the data necessary
 * to reconstruct a view exactly as it was when restoring a session.
 * <p>
 * This class is generic and can be specialized for any subclass of {@link KahinaView}.
 * @author jdellert
 *
 */
public abstract class KahinaViewConfiguration<T extends KahinaView<?>>  
{
	
	/**public static KahinaViewConfiguration importXML(Element e)
	{
		return new KahinaViewConfiguration<KahinaView<KahinaObject>>();
	}**/
	
	/**
	 * Default implementation creates an empty element without type specification
	 * @return
	 */
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:configuration");
		return el;
	}
}
