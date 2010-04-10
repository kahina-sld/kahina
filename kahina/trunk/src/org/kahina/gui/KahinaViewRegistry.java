package org.kahina.gui;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.KahinaException;
import org.kahina.data.KahinaObject;
import org.kahina.visual.KahinaView;

/**
 * TODO: import + export to XML file
 * 
 * @author johannes
 *
 */

public class KahinaViewRegistry
{
    static Map<Class<? extends KahinaObject>,Class<? extends KahinaView>> map = new HashMap<Class<? extends KahinaObject>,Class<? extends KahinaView>>();
    
    public static void registerMapping(Class<? extends KahinaObject> type, Class<? extends KahinaView> viewType)
    {
        if (viewType.getTypeParameters()[0].getClass().isAssignableFrom(type))
        {
            map.put(type, viewType);
        }
    }
    
    public static KahinaView generateViewFor(Class<?> type)
    {
        Class<? extends KahinaView> viewType = map.get(type);
        while (viewType == null)
        {
            type = type.getSuperclass();
            viewType = map.get(type);
        }      
        try
        {
            KahinaView view = viewType.newInstance();
            return view;
        }
        catch (InstantiationException e)
        {
            throw new KahinaException("fatal view registry error!", e);
        }
        catch (IllegalAccessException e)
        {
            throw new KahinaException("fatal view registry error!", e);
        }
    }
    
    //TODO: implement this
    public static void loadFromXMLFile(String fileName)
    {
        
    }
}
