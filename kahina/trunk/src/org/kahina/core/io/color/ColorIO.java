package org.kahina.core.io.color;

import java.awt.Color;

public class ColorIO
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
}
