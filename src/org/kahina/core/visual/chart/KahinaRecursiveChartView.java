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

public class KahinaRecursiveChartView  extends KahinaView<KahinaChart>
{
    private static final boolean verbose = false;
    
    KahinaChartViewConfiguration config;
    
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
        
        //determine necessary segment widths, for all edges including the offsets introduced by nesting
        HashMap<Integer,Integer> recursionOffsetLeft = new HashMap<Integer,Integer>();
        HashMap<Integer,Integer> recursionOffsetRight = new HashMap<Integer,Integer>();
        List<Integer> agenda = new LinkedList<Integer>();
        agenda.addAll(model.getDependencyRoots());
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
        }
        
        //determine necessary segment widths, taking the recursion offsets we just computed into account
        
        for (Integer rootEdge : model.getDependencyRoots())
        {
            //TODO: add up the dimensions of these components to determine overall chart size
            calculateCoordinates(rootEdge);
        }
    }
    
    private void calculateCoordinates(int edgeID)
    {
        if (config.decideEdgeDisplay(edgeID))
        {
            int leftBound = model.getLeftBoundForEdge(edgeID);
            int rightBound = model.getRightBoundForEdge(edgeID);
            String edgeCaption = model.getEdgeCaption(edgeID);
            Set<Integer> daughters = model.getDaughterEdgesForEdge(edgeID);
            //base case: no recursion, just calculate the space needed for the edge
            if (daughters.size() == 0)
            {     
                if (config.getCellWidthPolicy() != KahinaChartViewOptions.FIXED_WIDTH)
                {
                    Stroke edgeStroke = getEdgeStroke(edgeID);
                    Font edgeFont = getEdgeFont(edgeID);
                    
                    //determine minimum necessary cell dimensions
                    FontMetrics fm = getFontMetrics(edgeFont, edgeStroke, config.getZoomLevel());
                    int width = fm.stringWidth(edgeCaption) + 4;
                    
                    //process required cell dimension according to cell width policy:
                    //equally widen all spanned columns if width is larger than combined column width
                    int oldW = getNecessarySegmentWidthSum(leftBound, rightBound);
                    if (oldW < width)
                    {
                        distributeWidthOverSegments(leftBound, rightBound, width - oldW);
                    }
                }
            }
            //recursive case: space subsumes the space needed for the children
            else
            {
                
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
