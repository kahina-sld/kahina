package org.kahina.core.visual.chart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.visual.KahinaView;

public class KahinaRecursiveChartView  extends KahinaChartView
{
    private static final boolean verbose = false;
    
    KahinaChartViewConfiguration config;
    
    //display coordinates for edges (DO NOT generate these each time, or drawing will be slow)
    //unlike in the case of KahinaChartView, x and y coordinates are relative with respect to parent
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
    int chartWidth;

    public KahinaRecursiveChartView(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        g = null;
        
        config = new KahinaChartViewConfiguration();
        
        resetAllStructures();
        statusColorEncoding = new HashMap<Integer, Color>();
        statusHighlightColorEncoding = new HashMap<Integer, Color>();
        statusStrokeEncoding = new HashMap<Integer, Stroke>();
        statusFontEncoding = new HashMap<Integer, Font>();
        
        chartWidth = 0;
        cellHeight = 14;

        //setDisplayDecider(new KahinaChartEdgeDisplayDecider());

        kahina.registerInstanceListener("chart update", this);
    }
    
    public KahinaRecursiveChartView(KahinaChart chartModel, KahinaInstance<?, ?, ?, ?> kahina)
    {
        this(kahina);
        display(chartModel);
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
    
    private void resetAllStructures()
    {   
        
        edgeX = new HashMap<Integer, Integer>();
        edgeY = new HashMap<Integer, Integer>();
        height = new HashMap<Integer, Integer>();
        width = new HashMap<Integer, Integer>();
        
        segmentWidths = new HashMap<Integer,Integer>();
    }
    
    private void calculateCoordinates()
    {
        //data structure for segment widths determined by captions, font etc.
        segmentWidths  = new HashMap<Integer,Integer>();
        totalCellWidthMaximum = 0;
        
        //temporary data structure aligning edges with rows to be drawn in
        //HashMap<Integer,Integer> rowForEdge = new HashMap<Integer, Integer>();
        
        //cell height determined by font size       
        FontMetrics fm = getFontMetrics(new Font(Font.MONOSPACED,Font.PLAIN, config.getZoomLevel()), new BasicStroke(1), config.getZoomLevel());
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
        
        //determine recursion offsets introduced by nesting for all edges
        HashMap<Integer,Integer> recursionOffsetLeft = new HashMap<Integer,Integer>();
        HashMap<Integer,Integer> recursionOffsetRight = new HashMap<Integer,Integer>();
        List<Integer> agenda = new LinkedList<Integer>();
        agenda.addAll(model.getDependencyRoots());
        System.err.println("Dependency roots: " + model.getDependencyRoots());
        while (agenda.size() > 0)
        {
            int childID = agenda.remove(0);
            for (int parentID : model.getMotherEdgesForEdge(childID))
            {
                //offset grows whenever a child starts or ends at the same segment boundary as its parent
                if (model.getLeftBoundForEdge(childID) == model.getLeftBoundForEdge(parentID))
                {
                    int newOffsetLeft = recursionOffsetLeft.get(parentID) + 1;
                    Integer oldOffsetLeft = recursionOffsetLeft.get(childID);
                    if (oldOffsetLeft == null || newOffsetLeft > oldOffsetLeft)
                    {
                        recursionOffsetLeft.put(childID, newOffsetLeft);
                    }
                }
                if (model.getRightBoundForEdge(childID) == model.getRightBoundForEdge(parentID))
                {
                    int newOffsetRight = recursionOffsetRight.get(parentID) + 1;
                    Integer oldOffsetRight = recursionOffsetRight.get(childID);
                    if (oldOffsetRight == null || newOffsetRight > oldOffsetRight)
                    {
                        recursionOffsetRight.put(childID, newOffsetRight);
                    }
                }
            }
            //for any processed edge, the offsets must be defined (starting at 0 by default)
            if (recursionOffsetLeft.get(childID) == null) recursionOffsetLeft.put(childID, 0);
            if (recursionOffsetRight.get(childID) == null) recursionOffsetRight.put(childID, 0);
            
            agenda.addAll(model.getDaughterEdgesForEdge(childID));
        }
        
        //determine necessary segment widths, taking the recursion offsets we just computed into account
        for (int curEdge : model.getEdgeIDs())
        {
            if (config.decideEdgeDisplay(curEdge))
            {
                System.err.println("Now processing edge " + curEdge);
                int leftBound = model.getLeftBoundForEdge(curEdge);
                int rightBound = model.getRightBoundForEdge(curEdge);
                String edgeCaption = model.getEdgeCaption(curEdge);
                
                if (config.getCellWidthPolicy() != KahinaChartViewOptions.FIXED_WIDTH)
                {
                    Stroke edgeStroke = getEdgeStroke(curEdge);
                    Font edgeFont = getEdgeFont(curEdge);
                    
                    //determine minimum necessary cell width
                    fm = getFontMetrics(edgeFont, edgeStroke, config.getZoomLevel());
                    int cellWidth = fm.stringWidth(edgeCaption) + 4;
                    cellWidth += recursionOffsetLeft.get(curEdge) * config.getZoomLevel();
                    cellWidth += recursionOffsetRight.get(curEdge) * config.getZoomLevel();
                    width.put(curEdge, cellWidth);
                    
                    //process required cell dimension according to cell width policy:
                    //equally widen all spanned columns if width is larger than combined column width
                    int oldW = getNecessarySegmentWidthSum(leftBound, rightBound);
                    if (oldW < cellWidth)
                    {
                        distributeWidthOverSegments(leftBound, rightBound, cellWidth - oldW);
                    }
                }
            }
        }
        
        //stack the root components
        
        for (Integer rootEdge : model.getDependencyRoots())
        {
            
            calculateCoordinates(rootEdge);
            
        }
        
        //TODO: determine root component positions  
        
        //TODO: add up the dimensions of the root components to determine overall chart size
    }
    
    private void calculateCoordinates(int edgeID)
    {
        if (config.decideEdgeDisplay(edgeID))
        {
            int leftBound = model.getLeftBoundForEdge(edgeID);
            int rightBound = model.getRightBoundForEdge(edgeID);
            Set<Integer> daughters = model.getDaughterEdgesForEdge(edgeID);
            //base case: no recursion, just calculate and store the space needed for the edge
            if (daughters.size() == 0)
            {     
                height.put(edgeID, config.getZoomLevel());
            }
            //recursive case: space subsumes the space needed for the children
            else
            {
                ArrayList<HashMap<Integer,Integer>> usedSpace = new ArrayList<HashMap<Integer,Integer>>();
                //temporary data structure aligning edges with rows to be drawn in
                HashMap<Integer,Integer> rowForEdge = new HashMap<Integer, Integer>();
                
                //stacking the daughters locally according to stacking policy
                for (int curEdge : daughters)
                {
                    calculateCoordinates(curEdge);
                    
                    //determine vertical slot according to stacking policy
                    if (config.getEdgeStackingPolicy() == KahinaChartViewOptions.STACK_EDGES_FILL_SPACE)
                    {
                        //fit in as early as possible (start searching from the top)
                        for (int i = 0; true; i++)
                        {
                            if (isVacantRange(usedSpace, i, leftBound, rightBound))
                            {
                                reserveRange(usedSpace, i, leftBound, rightBound, curEdge);
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
                            if (!isVacantRange(usedSpace, j, leftBound, rightBound))
                            {
                                reserveRange(usedSpace, j + 1, leftBound, rightBound, curEdge);
                                rowForEdge.put(curEdge, j + 1);
                                break;
                            }
                        }
                    }
                }
                
                //TODO: determine daughter positions
                
                //TODO: add up daughter heights to determine height of the constituent
                
            }
        }
        else
        {
            edgeX.put(edgeID, 0);
            edgeY.put(edgeID, 0);
            height.put(edgeID, 0);
            width.put(edgeID, 0);
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
    
    private boolean isVacantRange(ArrayList<HashMap<Integer,Integer>> usedSpace, int rowID, int leftBound, int rightBound)
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
    
    private void reserveRange(ArrayList<HashMap<Integer,Integer>> usedSpace, int rowID, int leftBound, int rightBound, int edgeID)
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
    
    private int getNecessarySegmentWidthSum(int leftBound, int rightBound)
    {
        int widthSum = 0;
        for (int i = leftBound; i < rightBound; i++)
        {
            widthSum += getNecessarySegmentWidth(i);
        }
        return widthSum;
    }
}
