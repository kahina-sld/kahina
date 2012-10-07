package org.kahina.core.gui;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaInstance;
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

	public static KahinaView<?> generateViewFor(Class<?> type, KahinaInstance<?, ?, ?, ?> kahina)
	{
		Class<? extends KahinaView<?>> viewType = map.get(type);
		while (viewType == null)
		{
			type = type.getSuperclass();
			viewType = map.get(type);
		}
		KahinaView<?> view;
        Class<?> instanceClass = kahina.getClass();
        while (!instanceClass.equals(Object.class))
        {
            //System.err.println("instanceClass = " + instanceClass);
    		try
    		{
    			view = viewType.getConstructor(instanceClass).newInstance(kahina);
                return view;
    		} 
            catch (Exception e)
    		{
    			instanceClass = instanceClass.getSuperclass();
    		}
        }
		System.err.println("ERROR: KahinaViewRegistry did not find any usable view for type " + type);
        return null;
	}

	// TODO: implement this
	public static void loadFromXMLFile(String fileName)
	{

	}
}
