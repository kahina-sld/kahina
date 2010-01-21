package org.kahina.data.chart;

import java.util.List;
import java.util.Set;

import org.kahina.data.KahinaDbObject;
import org.kahina.io.database.KahinaDatabaseHandler;

public class KahinaDbChart extends KahinaChart implements KahinaDbObject
{
    KahinaDatabaseHandler db;
    
    public KahinaDbChart()
    {
        db = null;
    }
    
    public KahinaDbChart(KahinaDatabaseHandler db)
    {
        setDatabase(db);    
    }
    
    public void setDatabase(KahinaDatabaseHandler db)
    {
        this.db = db;
        //create table for KahinaCharts 
        //TODO: only do this if table does not exist yet
        db.execute("CREATE TABLE KahinaCharts" +
                   "(" +
                   "id INT," +
                   "leftBound INT," +
                   "rightBound INT," +
                   "leftmostCovered INT," +
                   "rightmostCovered INT," +
                   "PRIMARY KEY (id)" +
                   ");");
        //create a table for the segments of this particular chart
        db.execute("CREATE TABLE KahinaChartSegments" + this.id +
                "(" +
                "id INT," +
                "caption TEXT," +
                "PRIMARY KEY (id)" +
                ");");
        //create a table for the edges of this particular chart
        db.execute("CREATE TABLE KahinaChartEdges" + this.id +
                "(" +
                "id INT," +
                "leftBound INT," +
                "rightBound INT," +
                "caption TEXT," +
                "status INT," +
                "PRIMARY KEY (id)" +
                ");");      
    }
    
    public int getLeftmostCovered()
    {
        return db.queryInteger("SELECT leftmostCovered FROM KahinaCharts WHERE id = " + id + ";");     
    }
    
    public void setLeftmostCovered(int leftmostCovered)
    {
        db.update("UPDATE KahinaCharts SET leftmostCovered=" + leftmostCovered + " WHERE id=" + id + ";");
    }
    
    public int getRightmostCovered()
    {
        return db.queryInteger("SELECT rightmostCovered FROM KahinaCharts WHERE id = " + id + ";");     
    }
    
    public void setRightmostCovered(int rightmostCovered)
    {
        db.update("UPDATE KahinaCharts SET rightmostCovered=" + rightmostCovered + " WHERE id=" + id + ";");
    }
    
    public Set<Integer> getSegmentsWithCaption()
    {
        return db.queryInteger("SELECT rightBound FROM KahinaChartEdges" + id + " WHERE id=" + edgeID + ";");
    }
    
    public int getLeftBoundForEdge(int edgeID)
    {
        return db.queryInteger("SELECT leftBound FROM KahinaChartEdges" + id + " WHERE id=" + edgeID + ";");
    }
    
    public void setLeftBoundForEdge(int edgeID, int leftBound)
    {
        db.update("UPDATE KahinaChartEdges" + id + " SET leftBound=" + leftBound + " WHERE id=" + edgeID + ";");
    }
    
    public int getRightBoundForEdge(int edgeID)
    {
        return db.queryInteger("SELECT rightBound FROM KahinaChartEdges" + id + " WHERE id=" + edgeID + ";");
    }
    
    public void setRightBoundForEdge(int edgeID, int rightBound)
    {
        db.update("UPDATE KahinaChartEdges" + id + " SET rightBound=" + rightBound + " WHERE id=" + edgeID + ";");
    }
    
    public int getEdgeStatus(int edgeID)
    {
        return db.queryInteger("SELECT status FROM KahinaChartEdges" + id + " WHERE id=" + edgeID + ";");
    }
    
    public void setEdgeStatus(int edgeID, int status)
    {
        db.update("UPDATE KahinaChartEdges" + id + " SET status=" + status + " WHERE id=" + edgeID + ";");
    }
}
