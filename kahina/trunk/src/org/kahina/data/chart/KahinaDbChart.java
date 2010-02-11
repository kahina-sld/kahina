package org.kahina.data.chart;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;
import java.util.Set;

import org.kahina.core.KahinaException;
import org.kahina.io.database.DatabaseHandler;

public class KahinaDbChart extends KahinaChart
{
	private DatabaseHandler db;

	private PreparedStatement getLeftmostCoveredStatement;

	private PreparedStatement setLeftmostCoveredStatement;

	private PreparedStatement getRightmostCoveredStatement;

	private PreparedStatement setRightmostCoveredStatement;

	private PreparedStatement getLeftBoundStatement;

	private PreparedStatement setLeftBoundStatement;

	private PreparedStatement getRightBoundStatement;

	private PreparedStatement setRightBoundStatement;

	private PreparedStatement getSegmentsWithCaptionStatement;

	private PreparedStatement getSegmentCaptionStatement;

	private PreparedStatement updateSegmentCaptionStatement;

	private PreparedStatement insertSegmentCaptionStatement;

	private PreparedStatement getEdgeIDsStatement;

	private PreparedStatement getLeftBoundForEdgeStatement;

	private PreparedStatement updateLeftBoundForEdgeStatement;

	private PreparedStatement insertLeftBoundForEdgeStatement;

	private PreparedStatement getRightBoundForEdgeStatement;

	private PreparedStatement updateRightBoundForEdgeStatement;

	private PreparedStatement insertRightBoundForEdgeStatement;

	private PreparedStatement getEdgeStatusStatement;

	private PreparedStatement updateEdgeStatusStatement;

	private PreparedStatement insertEdgeStatusStatement;

	private PreparedStatement getEdgeCaptionStatement;

	private PreparedStatement updateEdgeCaptionStatement;

	private PreparedStatement insertEdgeCaptionStatement;

	public KahinaDbChart(DatabaseHandler db)
	{
		this.db = db;
		createTablesIfNecessary();
		createChart();
		prepareStatements();
		initialize();
	}

	public void createTablesIfNecessary()
	{
		String clientID = KahinaDbChart.class.getName();
		if (!db.isRegistered(clientID))
		{
			try
			{
				// create table for KahinaCharts
				db.execute("CREATE TABLE KahinaCharts " + "(" + "id INT,"
						+ "leftBound INT," + "rightBound INT,"
						+ "leftmostCovered INT," + "rightmostCovered INT,"
						+ "PRIMARY KEY (id)" + ")");
				// create a table for the chart segments
				db.execute("CREATE TABLE KahinaChartSegments" + "(" + "id INT,"
						+ "chart INT," + "caption LONG VARCHAR,"
						+ "PRIMARY KEY (id, chart)" + ")");
				// create a table for the chart edges
				db.execute("CREATE TABLE KahinaChartEdges" + "(" + "id INT,"
						+ "chart INT," + "leftBound INT," + "rightBound INT,"
						+ "caption LONG VARCHAR," + "status INT,"
						+ "PRIMARY KEY (id, chart)" + ")");
			} catch (SQLException e)
			{
				throw new KahinaException("Failed to create tables. ", e);
			}
			db.register(clientID);
		}
	}

	private void createChart()
	{
		try
		{
			db
					.execute("INSERT INTO KahinaCharts (id, leftBound, rightBound, leftmostCovered, rightmostCovered) VALUES ("
							+ getID() + ", 0, 0, 0, 0)");
		} catch (SQLException e)
		{
			throw new KahinaException("Failed to create chart.");
		}
	}

	private void prepareStatements()
	{
		int chartID = getID();
		getLeftmostCoveredStatement = db
				.prepareStatement("SELECT leftmostCovered FROM KahinaCharts WHERE id = "
						+ chartID);
		setLeftmostCoveredStatement = db
				.prepareStatement("UPDATE KahinaCharts SET leftmostCovered = ? WHERE id = "
						+ chartID);
		getRightmostCoveredStatement = db
				.prepareStatement("SELECT rightmostCovered FROM KahinaCharts WHERE id = "
						+ chartID);
		setRightmostCoveredStatement = db
				.prepareStatement("UPDATE KahinaCharts SET rightmostCovered = ? WHERE id = "
						+ chartID);
		getLeftBoundStatement = db
				.prepareStatement("SELECT leftBound FROM KahinaCharts WHERE id = "
						+ chartID);
		setLeftBoundStatement = db
				.prepareStatement("UPDATE KahinaCharts SET leftBound = ? WHERE id= "
						+ chartID);
		getRightBoundStatement = db
				.prepareStatement("SELECT rightBound FROM KahinaCharts WHERE id = "
						+ chartID);
		setRightBoundStatement = db
				.prepareStatement("UPDATE KahinaCharts SET leftBound= ? WHERE id = "
						+ chartID);
		getSegmentsWithCaptionStatement = db
				.prepareStatement("SELECT id FROM KahinaChartSegments WHERE chart = "
						+ chartID);
		getSegmentCaptionStatement = db
				.prepareStatement("SELECT caption FROM KahinaChartSegments WHERE id = ? AND chart = "
						+ chartID);
		updateSegmentCaptionStatement = db
				.prepareStatement("UPDATE KahinaChartSegments SET caption = ? WHERE id = ? AND chart = "
						+ chartID);
		insertSegmentCaptionStatement = db
				.prepareStatement("INSERT INTO KahinaChartSegments (id, chart, caption) VALUES (?, "
						+ chartID + ", ?)");
		getEdgeIDsStatement = db
				.prepareStatement("SELECT id FROM KahinaChartEdges WHERE chart= "
						+ chartID);
		getLeftBoundForEdgeStatement = db
				.prepareStatement("SELECT leftBound FROM KahinaChartEdges WHERE id = ? AND chart = "
						+ chartID);
		updateLeftBoundForEdgeStatement = db
				.prepareStatement("UPDATE KahinaChartEdges SET leftBound = ? WHERE id = ? AND chart = "
						+ chartID);
		insertLeftBoundForEdgeStatement = db
				.prepareStatement("INSERT INTO KahinaChartEdges (id, chart, leftBound) VALUES (?, "
						+ chartID + ", ?)");
		getRightBoundForEdgeStatement = db
				.prepareStatement("SELECT rightBound FROM KahinaChartEDGES WHERE id = ? AND chart = "
						+ chartID);
		updateRightBoundForEdgeStatement = db
				.prepareStatement("UPDATE KahinaChartEdges SET rightBound = ? WHERE id = ? AND CHART = "
						+ chartID);
		insertRightBoundForEdgeStatement = db
				.prepareStatement("INSERT INTO KahinaChartEdges (id, chart, rightBound) VALUES (?, "
						+ chartID + ", ?)");
		getEdgeStatusStatement = db
				.prepareStatement("SELECT status from KahinaChartEdges WHERE id = ? AND chart = "
						+ chartID);
		updateEdgeStatusStatement = db
				.prepareStatement("UPDATE KahinaChartEdges SET status = ? WHERE id = ? AND chart = "
						+ chartID);
		insertEdgeStatusStatement = db
				.prepareStatement("INSERT INTO KahinaChartEdges (id, chart, status) VALUES (?, "
						+ chartID + ", ?)");
		getEdgeCaptionStatement = db
				.prepareStatement("SELECT caption from KahinaChartEdges WHERE id = ? AND chart = "
						+ chartID);
		updateEdgeCaptionStatement = db
				.prepareStatement("UPDATE KahinaChartEdges SET caption = ? WHERE id = ? AND chart = "
						+ chartID);
		insertEdgeCaptionStatement = db
				.prepareStatement("INSERT INTO KahinaChartEdges (id, chart, caption) VALUES (?, "
						+ chartID + ", ?)");
	}

	public int getLeftmostCovered()
	{
		return db.queryInteger(getLeftmostCoveredStatement);
	}

	public void setLeftmostCovered(int leftmostCovered)
	{
		try
		{
			setLeftmostCoveredStatement.setInt(1, leftmostCovered);
			setLeftmostCoveredStatement.executeUpdate();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public int getRightmostCovered()
	{
		return db.queryInteger(getRightmostCoveredStatement);
	}

	public void setRightmostCovered(int rightmostCovered)
	{
		try
		{
			setRightmostCoveredStatement.setInt(1, rightmostCovered);
			setRightmostCoveredStatement.executeUpdate();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public int getLeftBound()
	{
		return db.queryInteger(getLeftBoundStatement);
	}

	public void setLeftBound(int leftBound)
	{
		try
		{
			setLeftBoundStatement.setInt(1, leftBound);
			setLeftBoundStatement.executeUpdate();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public int getRightBound()
	{
		return db.queryInteger(getRightBoundStatement);
	}

	public void setRightBound(int rightBound)
	{
		try
		{
			setRightBoundStatement.setInt(1, rightBound);
			setRightBoundStatement.executeUpdate();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public Set<Integer> getSegmentsWithCaption()
	{
		return db.queryIntSet(getSegmentsWithCaptionStatement);
	}

	public boolean segmentHasCaption(int segmentID)
	{
		return getSegmentCaption(segmentID) != null;
	}

	public String getSegmentCaption(int segmentID)
	{
		try
		{
			getSegmentCaptionStatement.setInt(1, segmentID);
			return db.queryString(getSegmentCaptionStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public void setSegmentCaption(int segmentID, String caption)
	{
		try
		{
			updateSegmentCaptionStatement.setString(1, caption);
			updateSegmentCaptionStatement.setInt(2, segmentID);
			if (updateSegmentCaptionStatement.executeUpdate() == 0)
			{
				insertSegmentCaptionStatement.setInt(1, segmentID);
				insertSegmentCaptionStatement.setString(2, caption);
				insertSegmentCaptionStatement.execute();
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public List<Integer> getEdgeIDs()
	{
		return db.queryIntList(getEdgeIDsStatement);
	}

	public int getLeftBoundForEdge(int edgeID)
	{
		try
		{
			getLeftBoundForEdgeStatement.setInt(1, edgeID);
			return db.queryInteger(getLeftBoundForEdgeStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public void setLeftBoundForEdge(int edgeID, int leftBound)
	{
		try
		{
			updateLeftBoundForEdgeStatement.setInt(1, leftBound);
			updateLeftBoundForEdgeStatement.setInt(2, edgeID);
			if (updateLeftBoundForEdgeStatement.executeUpdate() == 0)
			{
				insertLeftBoundForEdgeStatement.setInt(1, edgeID);
				insertLeftBoundForEdgeStatement.setInt(2, leftBound);
				insertLeftBoundForEdgeStatement.execute();
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public int getRightBoundForEdge(int edgeID)
	{
		try
		{
			getRightBoundForEdgeStatement.setInt(1, edgeID);
			return db.queryInteger(getRightBoundForEdgeStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public void setRightBoundForEdge(int edgeID, int rightBound)
	{
		try
		{
			updateRightBoundForEdgeStatement.setInt(1, rightBound);
			updateRightBoundForEdgeStatement.setInt(2, edgeID);
			if (updateRightBoundForEdgeStatement.executeUpdate() == 0)
			{
				insertRightBoundForEdgeStatement.setInt(1, edgeID);
				insertRightBoundForEdgeStatement.setInt(2, rightBound);
				insertRightBoundForEdgeStatement.execute();
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public int getEdgeStatus(int edgeID)
	{
		try
		{
			getEdgeStatusStatement.setInt(1, edgeID);
			return db.queryInteger(getEdgeStatusStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public void setEdgeStatus(int edgeID, int status)
	{
		try
		{
			updateEdgeStatusStatement.setInt(1, status);
			updateEdgeStatusStatement.setInt(2, edgeID);
			if (updateEdgeStatusStatement.executeUpdate() == 0)
			{
				insertEdgeStatusStatement.setInt(1, edgeID);
				insertEdgeStatusStatement.setInt(2, status);
				insertEdgeStatusStatement.execute();
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public String getEdgeCaption(int edgeID)
	{
		try
		{
			getEdgeCaptionStatement.setInt(1, edgeID);
			return db.queryString(getEdgeCaptionStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public void setEdgeCaption(int edgeID, String caption)
	{
		try
		{
			updateEdgeCaptionStatement.setString(1, caption);
			updateEdgeCaptionStatement.setInt(2, edgeID);
			if (updateEdgeCaptionStatement.executeUpdate() == 0)
			{
				insertEdgeCaptionStatement.setInt(1, edgeID);
				insertEdgeCaptionStatement.setString(2, caption);
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}
}
