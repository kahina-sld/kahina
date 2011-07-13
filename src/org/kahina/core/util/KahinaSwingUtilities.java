package org.kahina.core.util;

import java.awt.Point;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JViewport;

public class KahinaSwingUtilities
{
    public static void scrollToCenter(JScrollPane scrollPane, int x, int y)
    {
        JViewport viewport = scrollPane.getViewport();
        
        //TODO: instead, allow coordinates beyond object corner without producing artefacts
        int cornerX = x - viewport.getWidth() / 2;
        int cornerY = y - viewport.getHeight() / 2;
        if (cornerX < 0) cornerX = 0;
        if (cornerY < 0) cornerY = 0;
        

        Point p = new Point(cornerX, cornerY);
        try
        {
            //again: be careful because of weird swing errors
            viewport.setViewPosition(p);
        }
        catch (NullPointerException e)
        {
            System.err.println("Problems adapting scroll pane position: crash prevented.");
        }
    }

	public static Object visualError(String message, Throwable t)
	{
		return new Object[] { message, new JScrollPane(new JTextArea(Utilities.join(System.getProperty("line.separator"), Utilities.portrayStackTrace(t)), 20, 80)) };
	}

	public static JMenuItem createMenuItem(String text, String actionCommand, ActionListener actionListener)
	{
		JMenuItem result = new JMenuItem(text);
		result.setActionCommand(actionCommand);
		result.addActionListener(actionListener);
		return result;
	}
}
