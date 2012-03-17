package org.kahina.core.visual.chart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.visual.KahinaView;

public class KahinaChartView extends KahinaView<KahinaChart>
{
	private static final boolean verbose = false;
	
	KahinaChartViewConfiguration config;
    
    //keep track of occupied cells; also used for reverse indexing
    ArrayList<HashMap<Integer,Integer>> usedSpace;
    
    //display coordinates for edges (DO NOT generate these each time, or drawing will be slow)
    HashMap<Integer, Integer> edgeX;
    HashMap<Integer, Integer> edgeY;
    HashMap<Integer, Integer> height;
    HashMap<Integer, Integer> width;
    
    //mapping from status values to display properties
    HashMap<Integer, Color> statusColorEncoding;
    HashMap<Integer, Color> statusHighlightColorEncoding;
    HashMap<Integer, Stroke> statusStrokeEncoding;
    HashMap<Integer, Font> statusFontEncoding;
    
    //allow marking of a single edge in the chart
    private int markedEdge;
    
    //allow highlighting of a set of edges (depends on ancestors and descendants of current node)
    private Set<Integer> highlights = new HashSet<Integer>();
    
    //hack to allow precalculations from outside any drawing method
    private Graphics2D g;
    
    //private variables for internal calculations across functions
    int totalCellWidthMaximum;
    HashMap<Integer,Integer> segmentWidths;
    int cellHeight; //do not implement a setter for this, but change it with font size
    //temporary data structure for grid layout according to displayRangePolicy
    HashMap<Integer,Integer> segmentOffsets = new HashMap<Integer,Integer>();
    int chartWidth;
    
    public KahinaChartView(KahinaController control)
    {
    	super(control);
        g = null;
        
        config = new KahinaChartViewConfiguration();
        
        resetAllStructures();
        statusColorEncoding = new HashMap<Integer, Color>();
        statusHighlightColorEncoding = new HashMap<Integer, Color>();
        statusStrokeEncoding = new HashMap<Integer, Stroke>();
        statusFontEncoding = new HashMap<Integer, Font>();
        
        chartWidth = 0;
        cellHeight = 14;

        setDisplayDecider(new KahinaChartEdgeDisplayDecider());

        control.registerListener("chart update", this);
    }
    
    public KahinaChartView(KahinaChart chartModel, KahinaController control)
    {
        this(control);
        display(chartModel);
    }
    
    public KahinaChartViewConfiguration getConfig()
    {
    	return config;
    }
    
    public void display(KahinaChart chartModel)
    {
        this.model = chartModel;
        recalculate(); // TODO is this necessary?
    }
    
    @Override
	public void recalculate()
    {
        resetAllStructures();
        calculateCoordinates();
    }
    
    public void setDisplayDecider(KahinaChartEdgeDisplayDecider displayDecider)
    {
        this.config.displayDecider = displayDecider;
        displayDecider.setChartView(this);
    }
    
    
    private void resetAllStructures()
    {   
        usedSpace = new ArrayList<HashMap<Integer,Integer>>();
        
        edgeX = new HashMap<Integer, Integer>();
        edgeY = new HashMap<Integer, Integer>();
        height = new HashMap<Integer, Integer>();
        width = new HashMap<Integer, Integer>();
        
        segmentWidths = new HashMap<Integer,Integer>();
        segmentOffsets = new HashMap<Integer,Integer>();
    }
    
    private void calculateCoordinates()
    {
        //data structure for segment widths determined by captions, font etc.
        segmentWidths  = new HashMap<Integer,Integer>();
        totalCellWidthMaximum = 0;
        
        //temporary data structure aligning edges with rows to be drawn in
        HashMap<Integer,Integer> rowForEdge = new HashMap<Integer, Integer>();
        int fontSize = config.getZoomLevel();
        
        //cell height determined by font size       
        FontMetrics fm = getFontMetrics(new Font(Font.MONOSPACED,Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
        cellHeight = fm.getHeight();
        
        //initialize cell widths with values determined by their captions
        if (config.getCellWidthPolicy() != KahinaChartViewOptions.FIXED_WIDTH)
        {
            for (int id : model.getSegmentsWithCaption())
            {
                if (segmentDisplayed(id))
                {
                    int width = fm.stringWidth(id + " " + model.getSegmentCaption(id) + " ");
                    distributeWidthOverSegments(id, id + 1, width);
                }
            }
        }

        for (int curEdge : model.getEdgeIDs())
        {
            if (config.decideEdgeDisplay(curEdge))
            {
                int leftBound = model.getLeftBoundForEdge(curEdge);
                int rightBound = model.getRightBoundForEdge(curEdge);
                String edgeCaption = model.getEdgeCaption(curEdge);
                
                if (config.getCellWidthPolicy() != KahinaChartViewOptions.FIXED_WIDTH)
                {
                    Stroke edgeStroke = getEdgeStroke(curEdge);
                    Font edgeFont = getEdgeFont(curEdge);
                    
                    //determine minimum necessary cell dimensions
                    fm = getFontMetrics(edgeFont, edgeStroke, fontSize);
                    int width = fm.stringWidth(edgeCaption) + 4;
                    
                    //process required cell dimension according to cell width policy:
                    //equally widen all spanned columns if width is larger than combined column width
                    int oldW = getNecessarySegmentWidthSum(leftBound, rightBound);
                    if (oldW < width)
                    {
                        distributeWidthOverSegments(leftBound, rightBound, width - oldW);
                    }
                }
                
                //determine vertical slot according to stacking policy
                if (config.getEdgeStackingPolicy() == KahinaChartViewOptions.STACK_EDGES_FILL_SPACE)
                {
                    //fit in as early as possible (start searching from the top)
                    for (int i = 0; true; i++)
                    {
                        if (isVacantRange(i, leftBound, rightBound))
                        {
                            reserveRange(i, leftBound, rightBound, curEdge);
                            rowForEdge.put(curEdge, i);
                            break;
                        }
                    }
                }
                else //edgeStackingPolicy == STACK_EDGES_BY_ID
                {   
                    //fit in as late as necessary (start searching from the bottom)
                    for (int j = usedSpace.size() - 1; j >= -1; j--)
                    {
                        if (!isVacantRange(j, leftBound, rightBound))
                        {
                            reserveRange(j + 1, leftBound, rightBound, curEdge);
                            rowForEdge.put(curEdge, j + 1);
                            break;
                        }
                    }
                }
            }
        }
        
        int currentOffset = 0;
        if (config.getDisplayRangePolicy() == KahinaChartViewOptions.RANGE_COMPLETE)
        {
            for (int segmentID = 0; segmentID <= model.getRightmostCovered(); segmentID++)
            {
                segmentOffsets.put(segmentID, currentOffset);
                currentOffset += getSegmentWidth(segmentID);
            }
        }
        else
        {       
            HashSet<Integer> segmentIDs = new HashSet<Integer>();
            segmentIDs.addAll(segmentWidths.keySet());
            if (config.getDisplayRangePolicy() == KahinaChartViewOptions.RANGE_USED_OR_CAPTION_DEFINED)
            {
                segmentIDs.addAll(model.getSegmentsWithCaption());
            }
            ArrayList<Integer> sortedSegmentIDs = new ArrayList<Integer>();
            sortedSegmentIDs.addAll(segmentIDs);
            Collections.sort(sortedSegmentIDs);
            if (!sortedSegmentIDs.isEmpty())
            {
            	sortedSegmentIDs.add(sortedSegmentIDs.get(sortedSegmentIDs.size() - 1) + 1);
            }
            for (int segmentID : sortedSegmentIDs)
            {
                segmentOffsets.put(segmentID, currentOffset);
                currentOffset += getSegmentWidth(segmentID);
            }
        }
        chartWidth = currentOffset;
        
        if (verbose)
        {
        	System.err.println("Adapting edge coordinates...");
        }

        //second go: adapt all edge coordinates to determined column widths
        for (int curEdge : model.getEdgeIDs())
        {
        	if (verbose)
        	{
        		System.err.println("curEdge: " + curEdge);
        	}
            if (config.decideEdgeDisplay(curEdge))
            {
                //straightforward use of segmentOffsets to determine all the coordinates
                int drawIntoRow = rowForEdge.get(curEdge);               
                if (config.getDisplayOrientation() == KahinaChartViewOptions.BOTTOM_UP_DISPLAY)
                {
                    drawIntoRow = usedSpace.size() - drawIntoRow - 1;
                }   
                edgeY.put(curEdge, drawIntoRow * (cellHeight + 3));
                height.put(curEdge, cellHeight + 3);
                //System.err.println(segmentOffsets.get(model.getLeftBoundForEdge(curEdge)));
                int leftOffset = segmentOffsets.get(model.getLeftBoundForEdge(curEdge));
                if (verbose)
                {
                	System.err.println("Segment offsets: " + segmentOffsets);
                	if (segmentOffsets != null)
                	{
                		System.err.println("Segment offset: " + segmentOffsets.get(model.getRightBoundForEdge(curEdge)));
                	}
                    System.err.println("model.getRightBoundForEdge(" + curEdge + ") = " + model.getRightBoundForEdge(curEdge));
                    System.err.println("segmentOffsets.get(model.getRightBoundForEdge(curEdge) = " + segmentOffsets.get(model.getRightBoundForEdge(curEdge)));
                }
                int rightOffset = getSegmentOffset(model.getRightBoundForEdge(curEdge));
                //rightOffset += getSegmentWidth(model.getRightBoundForEdge(curEdge));
                edgeX.put(curEdge, leftOffset);
                width.put(curEdge, rightOffset - leftOffset);
            }
        }
    }
    
    public boolean segmentDisplayed(int id)
    {
        if (config.getDisplayRangePolicy() == KahinaChartViewOptions.RANGE_USED_OR_CAPTION_DEFINED)
        {
            return (model.segmentHasCaption(id) || model.segmentIsCovered(id));
        }
        else if (config.getDisplayRangePolicy() == KahinaChartViewOptions.RANGE_USED_ONLY)
        {
            return (model.segmentIsCovered(id));
        }
        return true;
    }
    
    private boolean isVacantRange(int rowID, int leftBound, int rightBound)
    {
        if (rowID < 0) return false;
        if (rowID >= usedSpace.size()) return true;
        else
        {
            HashMap<Integer,Integer> usedInRow = usedSpace.get(rowID);
            for (int i = leftBound; i < rightBound; i++)
            {
                if (usedInRow.get(i) != null) return false;
            }
            return true;
        }
    }
    
    private void reserveRange(int rowID, int leftBound, int rightBound, int edgeID)
    {
        while (rowID >= usedSpace.size())
        {
            usedSpace.add(new HashMap<Integer,Integer>());
        }
        HashMap<Integer,Integer> usedInRow = usedSpace.get(rowID);
        for (int i = leftBound; i < rightBound; i++)
        {
            usedInRow.put(i,edgeID);
        }
    }
    
    public int getNumberOfSegments()
    {
        return model.getRightmostCovered() - model.getLeftmostCovered();
    }
    
    /**
     * @param edgeID
     * @return highlight color for highlighted edges
     */
    public Color getEdgeColor(int edgeID)
    {
    	if (verbose)
    	{
    		System.err.print(this + ".getEdgeColor(" + edgeID + ")=");
    	}
        int status = model.getEdgeStatus(edgeID);
        Color col = null;
        if (highlights.contains(edgeID))
        {
        	if (verbose)
        	{
        		System.err.print("[hl]");
        	}
            col = statusHighlightColorEncoding.get(status);
            if (col == null)
            {
                col = Color.YELLOW;
            }
        }
        else
        {
        	if (verbose)
        	{
        		System.err.print("[nohl]");
        	}
            col = statusColorEncoding.get(status);
            if (col == null)
            {
                col = Color.WHITE;
            }
        }
        if (verbose)
        {
        	System.err.println(col);
        }
        return col;
    }
    
    /**
     * @param edgeID
     * @return always returns highlight color, even if edge is not highlighted
     */
    public Color getEdgeHighlightColor(int edgeID)
    {
        int status = model.getEdgeStatus(edgeID);
        Color col = statusHighlightColorEncoding.get(status);
        if (col == null)
        {
            return Color.YELLOW;
        }
        else
        {
            return col;
        }
    }
    
    public Stroke getEdgeStroke(int edgeID)
    {
        int status = model.getEdgeStatus(edgeID);
        Stroke str = statusStrokeEncoding.get(status);
        if (str == null)
        {
            return new BasicStroke(1);
        }
        else
        {
            return str;
        }
    }
    
    public Font getEdgeFont(int edgeID)
    {
        int status = model.getEdgeStatus(edgeID);
        Font fnt = statusFontEncoding.get(status);
        if (fnt == null)
        {
            return new Font(Font.SANS_SERIF,Font.PLAIN, config.getZoomLevel());
        }
        else
        {
            return new Font(fnt.getFamily(), fnt.getStyle(), config.getZoomLevel());
        }
    }
    
    public Iterable<Integer> getEdgeIDs()
    {
        return model.getEdgeIDs();
    }
    
    public Iterable<Integer> getVisibleEdgeIDs()
    {
        List<Integer> visibleEdges = new LinkedList<Integer>();
        for (int edge : model.getEdgeIDs())
        {
            if (config.decideEdgeDisplay(edge))
            {
                visibleEdges.add(edge);
            }
        }
        return visibleEdges;
    }
    
    public int getEdgeX(int edgeID)
    {
    	if (verbose)
    	{
    		System.err.println("Edge ID: " + edgeID);
    	}
        return edgeX.get(edgeID);
    }
    
    public int getEdgeY(int edgeID)
    {
        return edgeY.get(edgeID);
    }
    
    public int getEdgeHeight(int edgeID)
    {
        Integer edgeHeight = height.get(edgeID);
        if (edgeHeight == null)
        {
            return cellHeight;
        }
        return edgeHeight;
    }
    
    public int getEdgeWidth(int edgeID)
    {
        Integer edgeWidth = width.get(edgeID);
        if (edgeWidth == null)
        {
            return config.getCellWidth();
        }
        return edgeWidth;
    }
    
    public String getEdgeCaption(int edgeID)
    {
        return model.getEdgeCaption(edgeID);
    }
    
    public String getSegmentCaption(int segmentID)
    {
        return model.getSegmentCaption(segmentID);
    }
    
    public int getSegmentOffset(int segmentID)
    {
        Integer offset = segmentOffsets.get(segmentID);
        if (offset == null)
        {
            //simply return maximum offset
            for (int i = segmentID; i > 0; i--) 
            {
                offset = segmentOffsets.get(i);
                if (offset != null) return offset;
            }
            return 0;
        }
       return offset;
    }
    
    public FontMetrics getFontMetrics(Font f, Stroke s, int fontSize)
    {
        //hack to allow precalculations from outside any drawing method
        if ( g == null)
        {
            BufferedImage bufferedImage = new BufferedImage(2,2,BufferedImage.TYPE_4BYTE_ABGR_PRE);
            g =  bufferedImage.createGraphics();
        }
        g.setFont(new Font(f.getFontName(), f.getStyle(), fontSize));
        g.setStroke(s);
        return g.getFontMetrics();
    }
    
    public int getDisplayWidth()
    {
        return chartWidth;
    }
    
    public int getDisplayHeight()
    {
        return usedSpace.size() * (cellHeight + 3);
    }
    
    private int getNecessarySegmentWidthSum(int leftBound, int rightBound)
    {
        int widthSum = 0;
        for (int i = leftBound; i < rightBound; i++)
        {
            widthSum += getNecessarySegmentWidth(i);
        }
        return widthSum;
    }
    
    public int getSegmentWidth(int segmentID)
    {
        Integer width = segmentWidths.get(segmentID);
        if (width == null)
        {
            switch (config.getDisplayRangePolicy())
            {
                case KahinaChartViewOptions.RANGE_USED_OR_CAPTION_DEFINED:
                {
                    if (model.segmentHasCaption(segmentID))
                    {
                        if (config.getCellWidthPolicy() == KahinaChartViewOptions.FIXED_WIDTH) return config.getCellWidth();
                        else if (config.getCellWidthPolicy() == KahinaChartViewOptions.MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                    }
                    else
                    {
                        return 0;
                    }
                }
                case KahinaChartViewOptions.RANGE_USED_ONLY:
                {
                    return 0;
                }
                case KahinaChartViewOptions.RANGE_COMPLETE:
                {
                    if (config.getCellWidthPolicy() == KahinaChartViewOptions.FIXED_WIDTH) return config.getCellWidth();
                    else if (config.getCellWidthPolicy() == KahinaChartViewOptions.MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                    else return 0;
                }
            }
        }
        else
        {
            switch (config.getDisplayRangePolicy())
            {
                case KahinaChartViewOptions.RANGE_USED_OR_CAPTION_DEFINED:
                {
                    if (config.getCellWidthPolicy() ==  KahinaChartViewOptions.FIXED_WIDTH) return config.getCellWidth();
                    else if (config.getCellWidthPolicy() == KahinaChartViewOptions.MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                    else return width;
                }
                case KahinaChartViewOptions.RANGE_USED_ONLY:
                {
                    if (model.segmentIsCovered(segmentID))
                    {
                        if (config.getCellWidthPolicy() == KahinaChartViewOptions.FIXED_WIDTH) return config.getCellWidth();
                        else if (config.getCellWidthPolicy() == KahinaChartViewOptions.MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                        else return width;
                    }
                    else
                    {
                        return 0;
                    }
                }
                case KahinaChartViewOptions.RANGE_COMPLETE:
                {
                    if (config.getCellWidthPolicy() == KahinaChartViewOptions.FIXED_WIDTH) return config.getCellWidth();
                    else if (config.getCellWidthPolicy() == KahinaChartViewOptions.MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                    else return width;
                }
            }
        }
        return width;
    }
    
    //necessary for distributing widths because getSegmentWidths() reacts to liberal width policies
    private int getNecessarySegmentWidth(int segmentID)
    {
        Integer width = segmentWidths.get(segmentID);
        if (width == null)
        {
            return 0;
        }
        else
        {
            return width;
        }
    }
    
    private void distributeWidthOverSegments(int leftBound, int rightBound, int addedWidth)
    {
        int addedWidthPerSegment = addedWidth/(rightBound - leftBound);
        for (int i = leftBound; i < rightBound; i++)
        {
            int newWidth = getNecessarySegmentWidth(i) + addedWidthPerSegment;
            if (newWidth > totalCellWidthMaximum) totalCellWidthMaximum = newWidth;
            segmentWidths.put(i, newWidth);
        }
    }
    
    /**
     * Define the color of normal (non-highlighted) edges with a certain status.
     *
     * @param status - the status the color will be assigned to
     * @param color - the color for normal edges of that status
     */
    public void setStatusColorEncoding(int status, Color color)
    {
        statusColorEncoding.put(status, color);
    }
    
    /**
     * Define the color of highlighted edges with a certain status.
     *
     * @param status - the status the color will be assigned to
     * @param color - the color for highlighted edges of that status
     */
    public void setStatusHighlightColorEncoding(int status, Color color)
    {
        statusHighlightColorEncoding.put(status, color);
    }
    
    /**
     * Define the color encoding for a status.
     * Automatically generates a dimmer variant for normal edges
     * and a more saturated variant for highlighted edges.
     * More customizable variant: encode colors separately using
     * {@link #setStatusColorEncoding(int, Color)} and 
     * {@link #setStatusHighlightColorEncoding(int, Color)}.
     * @param status - the status the color will be assigned to
     * @param color - the base color to encode the status
     */
    public void setStatusAutoColorEncoding(int status, Color color)
    {
        statusColorEncoding.put(status, color.darker());
        statusHighlightColorEncoding.put(status, color.brighter());
    }
    
    public void setStatusFontEncoding(int status, Font font)
    {
        statusFontEncoding.put(status, font);
    }
    
    public int getMarkedEdge()
    {
        return markedEdge;
    }
    
    public void setMarkedEdge(int markedEdge)
    {
        this.markedEdge = markedEdge;
        updateHighlightings();
    }
    
    public void updateHighlightings()
    {
        highlights.clear();
        highlights.add(markedEdge);
        if (    config.getDependencyDisplayPolicy() == KahinaChartViewOptions.BOTH_ANCESTORS_AND_DESCENDANTS || 
        		config.getDependencyDisplayPolicy() == KahinaChartViewOptions.ANCESTORS_ONLY   )
        {
            //highlight ancestors
            LinkedList<Integer> agenda = new LinkedList<Integer>();
            agenda.addAll(model.getMotherEdgesForEdge(markedEdge));
            int nextAncestor = -1;
            while (agenda.size() > 0)
            {
                nextAncestor = agenda.remove(0);
                highlights.add(nextAncestor);
                if (config.transitiveAncestors)
                {
                    agenda.addAll(model.getMotherEdgesForEdge(nextAncestor));
                }
            }
        }
        if (    config.getDependencyDisplayPolicy() == KahinaChartViewOptions.BOTH_ANCESTORS_AND_DESCENDANTS || 
        		config.getDependencyDisplayPolicy() == KahinaChartViewOptions.DESCENDANTS_ONLY   )
        {
            //highlight descendants
            LinkedList<Integer> agenda = new LinkedList<Integer>();
            agenda.addAll(model.getDaughterEdgesForEdge(markedEdge));
            int nextAncestor = -1;
            while (agenda.size() > 0)
            {
                nextAncestor = agenda.remove(0);
                highlights.add(nextAncestor);
                if (config.transitiveDescendants)
                {
                    agenda.addAll(model.getDaughterEdgesForEdge(nextAncestor));
                }
            }
        }
    }
    
    public int edgeAtCoordinates(int x, int y)
    {
        //binary search over segment offsets
        ArrayList<Integer> sortedSegmentIDs = new ArrayList<Integer>();
        sortedSegmentIDs.addAll(segmentOffsets.keySet());
        Collections.sort(sortedSegmentIDs);
        int lowerIndex = 0;
        int upperIndex = sortedSegmentIDs.get(sortedSegmentIDs.size() - 1);
        int middleIndex = (int) Math.ceil((lowerIndex + upperIndex)/2.0);
        int middleSegment = sortedSegmentIDs.get(middleIndex);
        int middleBound = segmentOffsets.get(middleSegment);
        while (lowerIndex  + 1 != upperIndex)
        {
            //System.err.println("lower: " + lowerIndex + " upper: " + upperIndex + " middle bound: " + middleBound + " x: " + x);
            if (middleBound >= x)
            {
                upperIndex = middleIndex;
            }
            else
            {
                lowerIndex = middleIndex;
            }
            middleIndex = (lowerIndex + upperIndex)/2;
            middleSegment = sortedSegmentIDs.get(middleIndex);
            middleBound = segmentOffsets.get(middleSegment);
        }
        //System.err.println("Column: " + middleSegment);      
            
        //column determined via segment, now compute row
        if (config.getDisplayOrientation() == KahinaChartViewOptions.BOTTOM_UP_DISPLAY)
        {
            y = getDisplayHeight() - y; 
        }
        int row = y / (cellHeight + 3);

        //System.err.println("Row: " + row);
            
        //retrieve edge at that position in grid
        Integer result = usedSpace.get(row).get(middleSegment);
        if (result == null)
        {
            //System.err.println("No edge found!");
            return -1;
        }
        else
        {
            //System.err.println("Click on edge: " + result);
        }
        
        return result;
    }
    
    @Override
	public JComponent makePanel()
    {
        KahinaChartViewPanel panel = new KahinaChartViewPanel(control);
        control.registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
    
    //not interested in selection events or update events
    //because they always contain step information, not edge IDs
    @Override
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaChartUpdateEvent)
        {
            processEvent((KahinaChartUpdateEvent) e);
        }
    }
    
    protected void processEvent(KahinaChartUpdateEvent e)
    {
        setMarkedEdge(e.getSelectedEdge());
        recalculate();
    }
}

