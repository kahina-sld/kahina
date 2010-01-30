package org.kahina.data.chart;

import java.util.Set;

import org.kahina.data.KahinaDbObject;
import org.kahina.io.database.DatabaseHandler;

public class KahinaDbChart extends KahinaChart implements KahinaDbObject
{
    DatabaseHandler db;
    boolean tablesExist = false;
    
    public KahinaDbChart()
    {
        db = null;
    }
    
    public KahinaDbChart(DatabaseHandler db)
    {
        setDatabase(db); 
        initialize();
    }
    
    public void setDatabase(DatabaseHandler db)
    {
        this.db = db;
        if (!tablesExist)
        {
            //create table for KahinaCharts 
            db.execute("CREATE TABLE KahinaCharts " +
                       "(" +
                       "id INT," +
                       "leftBound INT," +
                       "rightBound INT," +
                       "leftmostCovered INT," +
                       "rightmostCovered INT," +
                       "PRIMARY KEY (id)" +
                       ");");
            //create a table for the chart segments
            db.execute("CREATE TABLE KahinaChartSegments" +
                    "(" +
                    "id INT," +
                    "chart INT," +
                    "caption TEXT," +
                    "PRIMARY KEY (id, chart)" +
                    ");");
            //create a table for the chart edges
            db.execute("CREATE TABLE KahinaChartEdges" +
                    "(" +
                    "id INT," +
                    "chart INT," +
                    "leftBound INT," +
                    "rightBound INT," +
                    "caption TEXT," +
                    "status INT," +
                    "PRIMARY KEY (id, chart)" +
                    ");");
            tablesExist = true;
        }
    }
    
    public int getLeftmostCovered()
    {
        return db.queryInteger("SELECT leftmostCovered FROM KahinaCharts WHERE id = " + getID() + ";");     
    }
    
    public void setLeftmostCovered(int leftmostCovered)
    {
        db.update("UPDATE KahinaCharts SET leftmostCovered=" + leftmostCovered + " WHERE id=" + getID() + ";");
    }
    
    public int getRightmostCovered()
    {
        return db.queryInteger("SELECT rightmostCovered FROM KahinaCharts WHERE id = " + getID() + ";");     
    }
    
    public void setRightmostCovered(int rightmostCovered)
    {
        db.update("UPDATE KahinaCharts SET rightmostCovered=" + rightmostCovered + " WHERE id=" + getID() + ";");
    }
    
    public int getLeftBound()
    {
        return db.queryInteger("SELECT leftBound FROM KahinaCharts WHERE id = " + getID() + ";");     
    }
    
    public void setLeftBound(int leftBound)
    {
        db.update("UPDATE KahinaCharts SET leftBound=" + leftBound + " WHERE id=" + getID() + ";");
    }
    
    public int getRightBound()
    {
        return db.queryInteger("SELECT rightBound FROM KahinaCharts WHERE id = " + getID() + ";");     
    }
    
    public void setRightBound(int rightBound)
    {
        db.update("UPDATE KahinaCharts SET leftBound=" + rightBound + " WHERE id=" + getID() + ";");
    }
    
    public Set<Integer> getSegmentsWithCaption()
    {
        return db.queryIntSet("SELECT id FROM KahinaChartSegments WHERE chart=" + getID() + ";");
    }
    
    public boolean segmentHasCaption(int segmentID)
    {
        return (getSegmentCaption(segmentID) != null);
    }
    
    public String getSegmentCaption(int segmentID)
    {
        return db.queryString("SELECT caption FROM KahinaChartSegments WHERE id=" + segmentID + " AND chart=" + getID() + ";");
    }
    
    public void setSegmentCaption(int segmentID, String caption)
    {
        db.update("INSERT INTO KahinaChartSegments (id,chart,caption) VALUES (" + segmentID + "," + getID() + ",\"" + caption + "\")  ON DUPLICATE KEY UPDATE;");
    }
    
    public Set<Integer> getEdgeIDs()
    {
        return db.queryIntSet("SELECT id FROM KahinaChartEdges WHERE chart=" + getID() + ";");
    }
    
    public int getLeftBoundForEdge(int edgeID)
    {
        return db.queryInteger("SELECT leftBound FROM KahinaChartEdges WHERE id=" + edgeID + " AND chart=" + getID() + ";");
    }
    
    public void setLeftBoundForEdge(int edgeID, int leftBound)
    {
        db.update("INSERT INTO KahinaChartEdges (id,chart,leftBound) VALUES (" + edgeID + "," + getID() + ",\"" + leftBound + "\") ON DUPLICATE KEY UPDATE;");
    }
    
    public int getRightBoundForEdge(int edgeID)
    {
        return db.queryInteger("SELECT rightBound FROM KahinaChartEdges WHERE id=" + edgeID + " AND chart=" + getID() + ";");
    }
    
    public void setRightBoundForEdge(int edgeID, int rightBound)
    {
        db.update("INSERT INTO KahinaChartEdges (id,chart,rightBound) VALUES (" + edgeID + "," + getID() + ",\"" + rightBound + "\") ON DUPLICATE KEY UPDATE;");
    }
    
    public int getEdgeStatus(int edgeID)
    {
        return db.queryInteger("SELECT status FROM KahinaChartEdges WHERE id=" + edgeID + " AND chart=" + getID() + ";");
    }
    
    public void setEdgeStatus(int edgeID, int status)
    {
        db.update("INSERT INTO KahinaChartEdges (id,chart,status) VALUES (" + edgeID + "," + getID() + ",\"" + status + "\") ON DUPLICATE KEY UPDATE;");
    }
    
    public String getEdgeCaption(int edgeID)
    {
        return db.queryString("SELECT caption FROM KahinaChartEdges WHERE id=" + edgeID + " AND chart=" + getID() + ";");
    }
    
    public void setEdgeCaption(int edgeID, String caption)
    {
        db.update("INSERT INTO KahinaChartEdges (id,chart,caption) VALUES (" + edgeID + "," + getID() + ",\"" + caption + "\") ON DUPLICATE KEY UPDATE;");
    }
}
