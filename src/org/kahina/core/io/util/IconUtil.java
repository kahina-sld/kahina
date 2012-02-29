package org.kahina.core.io.util;

import java.net.URL;

import org.kahina.core.KahinaInstance;

public class IconUtil {
    
    public static URL getIcon(String filename) {
	return KahinaInstance.class.getResource(filename);
    }

}
