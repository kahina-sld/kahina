package org.kahina.core.io.util;

import java.net.URL;

public class IconUtil {
    
    public static URL getIcon(String filename) {
	return IconUtil.class.getResource(filename);
    }

}
