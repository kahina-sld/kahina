package org.kahina.core.util;

import java.awt.Point;

import javax.swing.JScrollPane;
import javax.swing.JViewport;

public class SwingUtilities
{
    public static void scrollToCenter(JScrollPane scrollPane, int x, int y)
    {
        JViewport viewport = scrollPane.getViewport();
        Point p = new Point(x - viewport.getWidth() / 2, y - viewport.getHeight() / 2);
        viewport.setViewPosition(p);
    }
}
