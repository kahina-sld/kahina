package org.kahina.core.io.color;

import java.awt.Color;

public class ColorUtil
{
    public static Color decodeHTML(String htmlColorString)
    {
        Color c = null;
        try 
        {
          c = Color.decode(htmlColorString.trim());
        } 
        catch (NumberFormatException e) 
        {
          throw new IllegalArgumentException("ERROR DECODING COLOR IN HTML FORMAT");
        }
        return c;
    }
      
    public static String encodeHTML(Color color)
    {
      return "#" + Integer.toHexString(color.getRGB()).substring(2).toUpperCase();
    }
    
    public static Color randomColor()
    {
        int r = (int) (Math.random() * 256);
        int g = (255 - r) + (int) (Math.random() * r);
        int b = 510 - r - g;
        return new Color(r,g,b);
    }
}
