package org.kahina.core.data.dag;

import java.awt.Color;
import java.util.LinkedList;
import java.util.List;

public class ColoredPath
{
    private Color color;
    private LinkedList<Integer> path;
    
    /**
     * Creates an empty path of the specified color.
     * @param color the color of the new path
     */
    public ColoredPath(Color color)
    {
        this.color = color;
        this.path = new LinkedList<Integer>();
    }
    
    /**
     * Creates a specified path of the specified color.
     * @param path a list of integers representing the path
     * @param color the color of the new path
     */
    public ColoredPath(List<Integer> path, Color color)
    {
        this.color = color;
        this.path = new LinkedList<Integer>();
        this.path.addAll(path);
    }
    
    /**
     * Extends the path by a numeric step ID.
     * @param id the Id by which the path is extended
     */
    public void append(int id)
    {
        path.add(id);
    }
    
    /**
     * Gets the color of the path.
     * @return a Color object representing the path's color
     */
    public Color getColor()
    {
        return color;
    }
    
    /**
     * Changes the color of the path.
     * @param color a Color object representing the new color of the path
     */
    public void setColor(Color color)
    {
        this.color = color;
    }
    
    public List<Integer> getPath()
    {
        return path;
    }
}
