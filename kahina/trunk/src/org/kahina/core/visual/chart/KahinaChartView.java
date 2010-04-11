package org.kahina.core.visual.chart;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.*;

import org.kahina.core.data.*;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;

public class KahinaChartView extends KahinaView<KahinaChart>
{
    //display options
    private int cellWidth = 150; 
    Color bgColor = Color.WHITE;
    int cellWidthPolicy = KahinaChartView.MINIMAL_NECESSARY_WIDTH;
    int edgeStackingPolicy = KahinaChartView.STACK_EDGES_FILL_SPACE;
    int displayOrientation = KahinaChartView.BOTTOM_UP_DISPLAY;
    int displayRangePolicy = KahinaChartView.RANGE_COMPLETE;
    int antialiasingPolicy = KahinaChartView.ANTIALIASING;
    HashMap<Integer,Boolean> statusDisplayed;
    int fontSize; //also determines zoom factor and cell height
    int cellHeight; //do not implement a setter for this, but change it with font size
        
    //DISPLAY CONSTANTS
    
    //possible values for cellWidthPolicy
    public static final int FIXED_WIDTH = 0;
    public static final int MINIMAL_NECESSARY_WIDTH = 1;
    public static final int MAXIMAL_NECESSARY_WIDTH = 2;
    
    //possible values for edgeStackingPolicy
    public static final int STACK_EDGES_FILL_SPACE = 0;
    public static final int STACK_EDGES_BY_ID = 1;
    
    //possible values for displayOrientation
    public static final int BOTTOM_UP_DISPLAY = 0;
    public static final int TOP_DOWN_DISPLAY = 1;
    
    //possible values for displayRangePolicy
    public static final int RANGE_USED_OR_CAPTION_DEFINED = 0;
    public static final int RANGE_USED_ONLY = 1;
    public static final int RANGE_COMPLETE = 2;
    
    //possible values for antialiasing policy
    public static final int ANTIALIASING = 0;
    public static final int NO_ANTIALIASING = 1;
    
    //keep track of occupied cells; also used for reverse indexing
    ArrayList<HashMap<Integer,Integer>> usedSpace;
    
    //display coordinates for edges (DO NOT generate these each time, or drawing will be slow)
    HashMap<Integer, Integer> edgeX;
    HashMap<Integer, Integer> edgeY;
    HashMap<Integer, Integer> height;
    HashMap<Integer, Integer> width;
    
    //mapping from status values to display properties
    HashMap<Integer, Color> statusColorEncoding;
    HashMap<Integer, Stroke> statusStrokeEncoding;
    HashMap<Integer, Font> statusFontEncoding;
    
    //hack to allow precalculations from outside any drawing method
    private Graphics2D g;
    
    //private variables for internal calculations across functions
    int totalCellWidthMaximum;
    HashMap<Integer,Integer> segmentWidths;
    //temporary data structure for grid layout according to displayRangePolicy
    HashMap<Integer,Integer> segmentOffsets = new HashMap<Integer,Integer>();
    int chartWidth;
    
    public KahinaChartView()
    {
        g = null;
        resetAllStructures();
        statusColorEncoding = new HashMap<Integer, Color>();
        statusStrokeEncoding = new HashMap<Integer, Stroke>();
        statusFontEncoding = new HashMap<Integer, Font>();
        chartWidth = 0;
        fontSize = 10;
        cellHeight = 14;
    }
    
    public void display(KahinaChart chartModel)
    {
        this.model = chartModel;
        resetAllStructures();
        calculateCoordinates();
    }
    
    public void zoomIn()
    {
        if (fontSize < 20)
        {
            fontSize += 1;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("No zoom levels beyond 20 allowed!");
        }
    }
    
    public void zoomOut()
    {
        if (fontSize > 4)
        {
            fontSize -= 1;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("No zoom levels below 4 allowed!");
        }
    }
    
    public void setZoomLevel(int level)
    {
        fontSize = level;
        resetAllStructures();
        calculateCoordinates();
    }
    
    public int getZoomLevel()
    {
        return fontSize;
    }
    
    public void setCellWidthPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            cellWidthPolicy = newPolicy;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("WARNING: unknown cell width policy value " + newPolicy);
        }
    }
    
    public void setDisplayOrientation(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            displayOrientation = newPolicy;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("WARNING: unknown displayOrientation value " + newPolicy);
        }
    }
    
    public void setDisplayRangePolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            displayRangePolicy = newPolicy;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("WARNING: unknown display range policy value " + newPolicy);
        }
    }
    
    public void setEdgeStackingPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            edgeStackingPolicy = newPolicy;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("WARNING: unknown edge stacking policy value " + newPolicy);
        }
    }
    
    public int getCellWidthPolicy()
    {
        return cellWidthPolicy;
    }
    
    public int getEdgeStackingPolicy()
    {
        return edgeStackingPolicy;
    }
    
    public int getDisplayOrientation()
    {
        return displayOrientation;
    }
    
    public int getDisplayRangePolicy()
    {
        return displayRangePolicy;
    }
    
    public int getAntialiasingPolicy()
    {
        return antialiasingPolicy;
    }

    public void setAntialiasingPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            antialiasingPolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown antialiasing policy value " + newPolicy);
        }
    }
    
    private void resetAllStructures()
    {
        statusDisplayed = new HashMap<Integer,Boolean>();
        
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
        
        //cell height determined by font size       
        FontMetrics fm = getFontMetrics(new Font(Font.MONOSPACED,Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
        cellHeight = fm.getHeight();
        
        //initialize cell widths with values determined by their captions
        if (cellWidthPolicy != FIXED_WIDTH)
        {
            for (int id : model.getSegmentsWithCaption())
            {
                if (segmentDisplayed(id))
                {
                    int width = fm.stringWidth(id + " " + model.getSegmentCaption(id) + " ");
                    distributeWidthOverSegments(id, id, width);
                }
            }
        }

        for (int curEdge : model.getEdgeIDs())
        {
            if (decideEdgeDisplayByStatus(curEdge))
            {
                int leftBound = model.getLeftBoundForEdge(curEdge);
                int rightBound = model.getRightBoundForEdge(curEdge);
                String edgeCaption = model.getEdgeCaption(curEdge);
                
                if (cellWidthPolicy != FIXED_WIDTH)
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
                if (edgeStackingPolicy == STACK_EDGES_FILL_SPACE)
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
        if (displayRangePolicy == RANGE_COMPLETE)
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
            if (displayRangePolicy == RANGE_USED_OR_CAPTION_DEFINED)
            {
                segmentIDs.addAll(model.getSegmentsWithCaption());
            }
            ArrayList<Integer> sortedSegmentIDs = new ArrayList<Integer>();
            sortedSegmentIDs.addAll(segmentIDs);
            Collections.sort(sortedSegmentIDs);
            for (int segmentID : sortedSegmentIDs)
            {
                segmentOffsets.put(segmentID, currentOffset);
                currentOffset += getSegmentWidth(segmentID);
            }
        }
        chartWidth = currentOffset;

        //second go: adapt all edge coordinates to determined column widths
        for (int curEdge : model.getEdgeIDs())
        {
            if (decideEdgeDisplayByStatus(curEdge))
            {
                //straightforward use of segmentOffsets to determine all the coordinates
                int drawIntoRow = rowForEdge.get(curEdge);               
                if (displayOrientation == BOTTOM_UP_DISPLAY)
                {
                    drawIntoRow = usedSpace.size() - drawIntoRow - 1;
                }   
                edgeY.put(curEdge, drawIntoRow * (cellHeight + 3));
                height.put(curEdge, cellHeight + 3);
                int leftOffset = segmentOffsets.get(model.getLeftBoundForEdge(curEdge));
                int rightOffset = segmentOffsets.get(model.getRightBoundForEdge(curEdge));
                rightOffset += getSegmentWidth(model.getRightBoundForEdge(curEdge));
                edgeX.put(curEdge, leftOffset);
                width.put(curEdge, rightOffset - leftOffset);
            }
        }
    }
    
    public boolean segmentDisplayed(int id)
    {
        if (displayRangePolicy == RANGE_USED_OR_CAPTION_DEFINED)
        {
            return (model.segmentHasCaption(id) || model.segmentIsCovered(id));
        }
        else if (displayRangePolicy == RANGE_USED_ONLY)
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
            for (int i = leftBound; i <= rightBound; i++)
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
        for (int i = leftBound; i <= rightBound; i++)
        {
            usedInRow.put(i,edgeID);
        }
    }
    
    private boolean decideEdgeDisplayByStatus(int status)
    {
        if (statusDisplayed.get(status) == null)
        {
            return true;
        }
        else
        {
            return statusDisplayed.get(status);
        }
    }
    
    public int getNumberOfSegments()
    {
        return model.getRightmostCovered() - model.getLeftmostCovered();
    }
    
    public Color getEdgeColor(int edgeID)
    {
        int status = model.getEdgeStatus(edgeID);
        Color col = statusColorEncoding.get(status);
        if (col == null)
        {
            return Color.WHITE;
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
            return new Font(Font.SANS_SERIF,Font.PLAIN, fontSize);
        }
        else
        {
            return new Font(fnt.getFamily(), fnt.getStyle(), fontSize);
        }
    }
    
    public Iterable<Integer> getEdgeIDs()
    {
        return model.getEdgeIDs();
    }
    
    public int getEdgeX(int edgeID)
    {
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
            return cellWidth;
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
        if (offset == null) return 0;
        else return offset;
    }
    
    public FontMetrics getFontMetrics(Font f, Stroke s, int fontSize)
    {
        //hack to allow precalculations from outside any drawing method
        if ( g == null)
        {
            BufferedImage bufferedImage = new BufferedImage(2,2,BufferedImage.TYPE_4BYTE_ABGR_PRE);
            g =  (Graphics2D) bufferedImage.createGraphics();
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
        for (int i = leftBound; i <= rightBound; i++)
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
            switch (displayRangePolicy)
            {
                case RANGE_USED_OR_CAPTION_DEFINED:
                {
                    if (model.segmentHasCaption(segmentID))
                    {
                        if (cellWidthPolicy == FIXED_WIDTH) return cellWidth;
                        else if (cellWidthPolicy == MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                    }
                    else
                    {
                        return 0;
                    }
                }
                case RANGE_USED_ONLY:
                {
                    return 0;
                }
                case RANGE_COMPLETE:
                {
                    if (cellWidthPolicy == FIXED_WIDTH) return cellWidth;
                    else if (cellWidthPolicy == MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                    else return 0;
                }
            }
        }
        else
        {
            switch (displayRangePolicy)
            {
                case RANGE_USED_OR_CAPTION_DEFINED:
                {
                    if (cellWidthPolicy ==  FIXED_WIDTH) return cellWidth;
                    else if (cellWidthPolicy == MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                    else return width;
                }
                case RANGE_USED_ONLY:
                {
                    if (model.segmentIsCovered(segmentID))
                    {
                        if (cellWidthPolicy == FIXED_WIDTH) return cellWidth;
                        else if (cellWidthPolicy == MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
                        else return width;
                    }
                    else
                    {
                        return 0;
                    }
                }
                case RANGE_COMPLETE:
                {
                    if (cellWidthPolicy == FIXED_WIDTH) return cellWidth;
                    else if (cellWidthPolicy == MAXIMAL_NECESSARY_WIDTH) return totalCellWidthMaximum;
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
        int addedWidthPerSegment = addedWidth/(rightBound + 1 - leftBound) + 1;
        for (int i = leftBound; i <= rightBound; i++)
        {
            int newWidth = getNecessarySegmentWidth(i) + addedWidthPerSegment;
            if (newWidth > totalCellWidthMaximum) totalCellWidthMaximum = newWidth;
            segmentWidths.put(i, newWidth);
        }
    }
    
    public void setStatusColorEncoding(int status, Color color)
    {
        statusColorEncoding.put(status, color);
    }
    
    public void setStatusFontEncoding(int status, Font font)
    {
        statusFontEncoding.put(status, font);
    }

    public void setCellWidth(int cellWidth)
    {
        this.cellWidth = cellWidth;
    }

    public int getCellWidth()
    {
        return cellWidth;
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
        if (displayOrientation == BOTTOM_UP_DISPLAY)
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
    
    public KahinaChartViewPanel wrapInPanel()
    {
        KahinaChartViewPanel panel = new KahinaChartViewPanel();
        panel.setView(this);
        return panel;
    }
}

