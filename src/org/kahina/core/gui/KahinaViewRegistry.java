package org.kahina.core.gui;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.KahinaException;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.visual.KahinaView;

/**
 * TODO: import + export to XML file
 * 
 * @author johannes
 * 
 */

public class KahinaViewRegistry
{
	static Map<Class<? extends KahinaObject>, Class<? extends KahinaView<?>>> map = new HashMap<Class<? extends KahinaObject>, Class<? extends KahinaView<?>>>();

	public static <T extends KahinaObject> void registerMapping(Class<T> type, Class<? extends KahinaView<? super T>> viewType)
	{
		map.put(type, viewType);
	}

	public static KahinaView<?> generateViewFor(Class<?> type, KahinaController control)
	{
		Class<? extends KahinaView<?>> viewType = map.get(type);
		while (viewType == null)
		{
			type = type.getSuperclass();
			viewType = map.get(type);
		}
		KahinaView<?> view;
		try
		{
			view = viewType.getConstructor(KahinaController.class).newInstance(control);
		} catch (Exception e)
		{
			throw new KahinaException("Problem generating view for " + type, e);
		}
		return view;
	}

	// TODO: implement this
	public static void loadFromXMLFile(String fileName)
	{

	}
}
