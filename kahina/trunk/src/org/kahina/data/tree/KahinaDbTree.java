package org.kahina.data.tree;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;

import org.kahina.core.KahinaException;
import org.kahina.io.database.DatabaseHandler;

public class KahinaDbTree extends KahinaTree
{
	private static final String CLIENT_ID = KahinaDbTree.class.getName();

	private static final String TABLE_NAME_PREFIX = KahinaDbTree.class
			.getSimpleName()
			+ "_";

	private static final String NODE_TABLE_NAME = TABLE_NAME_PREFIX + "nodes";

	private DatabaseHandler db;

	private PreparedStatement addNodeStatement;

	private PreparedStatement addEdgeStatement;

	private PreparedStatement addLayerInformationStatement;

	private PreparedStatement getRealParentStatement;

	private PreparedStatement getVirtualParentStatement;

	private PreparedStatement getLayerStatement;

	private PreparedStatement clearStatement;

	private PreparedStatement collapseStatement;

	private PreparedStatement decollapseStatement;

	private PreparedStatement decollapseAllStatement;

	private PreparedStatement getRealChildrenStatement;

	private PreparedStatement getVirtualChildrenStatement;

	private PreparedStatement getEdgeLabelStatement;

	private PreparedStatement getNodeCaptionStatement;

	private PreparedStatement getNodeStatusStatement;

	private PreparedStatement getRootStatement;

	private PreparedStatement isCollapsedStatement;

	private PreparedStatement getSizeStatement;

	private int nextID;

	public KahinaDbTree(DatabaseHandler db)
	{
		this(new DefaultLayerDecider(), db);
	}

	public KahinaDbTree(LayerDecider decider, DatabaseHandler db)
	{
		super(decider);
		this.db = db;
		createTablesIfNecessary();
		prepareStatements();
		nextID = 0;
	}

	private void createTablesIfNecessary()
	{
		if (!db.isRegistered(CLIENT_ID))
		{
			db.execute("CREATE TABLE " + NODE_TABLE_NAME + " (id INT, "
					+ "tree INT, " + "nodeCaption LONG VARCHAR,"
					+ " edgeLabel LONG VARCHAR," + " collapsed TINYINT(1),"
					+ " realParent INT," + " layer INT,"
					+ " virtualParent INT," + " PRIMARY KEY (id, tree) "
					+ "INDEX realParent (realParent), "
					+ "INDEX layer (layer), "
					+ "INDEX virtualParent (virtualParent))");
		}
	}

	private void prepareStatements()
	{
		int treeID = getID();
		addNodeStatement = db.prepareStatement("INSERT INTO " + NODE_TABLE_NAME
				+ " (id, tree, nodeCaption, edgeLabel) VALUES (?, " + treeID
				+ ", ?, ?)");
		addEdgeStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
				+ " SET realParent = ? WHERE id = ? AND tree = " + treeID);
		addLayerInformationStatement = db.prepareStatement("UPDATE "
				+ NODE_TABLE_NAME
				+ " SET layer = ?, virtualParent = ? WHERE id = ? AND tree = "
				+ treeID);
		getRealParentStatement = db.prepareStatement("SELECT realParent FROM "
				+ NODE_TABLE_NAME + " WHERE id = ? AND tree = " + treeID);
		getVirtualParentStatement = db
				.prepareStatement("SELECT virtualParent FROM "
						+ NODE_TABLE_NAME + " WHERE id = ? AND tree = "
						+ treeID);
		clearStatement = db.prepareStatement("DELETE FROM " + NODE_TABLE_NAME
				+ " WHERE tree = " + treeID);
		collapseStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
				+ " SET collapsed = 1 WHERE id = ? AND tree = " + treeID);
		decollapseStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
				+ " SET collapsed = 0 WHERE id = ? AND tree = " + treeID);
		decollapseAllStatement = db
				.prepareStatement("UPDATE " + NODE_TABLE_NAME
						+ " SET collapsed = 0 WHERE tree = " + treeID);
		getRealChildrenStatement = db.prepareStatement("SELECT id FROM "
				+ NODE_TABLE_NAME + " WHERE realParent = ? AND tree = "
				+ treeID);
		getVirtualChildrenStatement = db.prepareStatement("SELECT id FROM "
				+ NODE_TABLE_NAME + " WHERE virtualParent = ? AND tree = "
				+ treeID);
		getEdgeLabelStatement = db.prepareStatement("SELECT edgeLabel FROM "
				+ NODE_TABLE_NAME + " WHERE id = ? AND tree = " + treeID);
		getNodeCaptionStatement = db
				.prepareStatement("SELECT nodeCaption FROM " + NODE_TABLE_NAME
						+ " WHERE id = ? AND tree = " + treeID);
		getRootStatement = db.prepareStatement("SELECT id FROM "
				+ NODE_TABLE_NAME + " WHERE realParent IS NULL AND tree = "
				+ treeID);
		isCollapsedStatement = db.prepareStatement("SELECT collapsed FROM "
				+ NODE_TABLE_NAME + " WHERE id = ? AND tree = " + treeID);
		getSizeStatement = db.prepareStatement("SELECT COUNT(*) FROM "
				+ NODE_TABLE_NAME + " WHERE tree = " + treeID);
	}

	@Override
	public int addNode(String caption, String label, int nodeStatus)
	{
		int id = nextID++;
		try
		{
			addNodeStatement.setInt(1, id);
			addNodeStatement.setString(2, caption);
			addNodeStatement.setString(3, label);
			addNodeStatement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return id;
	}

	@Override
	public void addChild(int parent, int child)
	{
		try
		{
			addEdgeStatement.setInt(1, parent);
			addEdgeStatement.setInt(2, child);
			addEdgeStatement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		computeAndStoreLayerInformation(child);
	}
	
	private void computeAndStoreLayerInformation(int child)
	{
		int layer = decider.decideOnLayer(child, this);
		int virtualParent = getParent(child);
		while (getLayer(virtualParent) > layer)
		{
			virtualParent = getVirtualParent(virtualParent);
		}
		try
		{
			addLayerInformationStatement.setInt(1, layer);
			addLayerInformationStatement.setInt(2, virtualParent);
			addLayerInformationStatement.setInt(3, child);
			addLayerInformationStatement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	@Override
	public void setLayerDecider(LayerDecider decider)
	{
		super.setLayerDecider(decider);
		recomputeLayers();
	}

	public void recomputeLayers()
	{
		recomputeLayers(getRootID());
	}

	public void recomputeLayers(int nodeID)
	{
		computeAndStoreLayerInformation(nodeID);
		for (int childID : getChildren(nodeID))
		{
			recomputeLayers(childID);
		}
	}

	@Override
	public int getParent(int nodeID)
	{
		try
		{
			getRealParentStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryInteger(getRealParentStatement, -1);
	}

	private int getVirtualParent(int nodeID)
	{
		try
		{
			getVirtualParentStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryInteger(getVirtualParentStatement, -1);
	}

	private int getLayer(int nodeID)
	{
		try
		{
			getLayerStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryInteger(getLayerStatement, 0);
	}

	@Override
	public void clear()
	{
		db.execute(clearStatement);
	}

	@Override
	public void collapse(int nodeID)
	{
		try
		{
			collapseStatement.setInt(1, nodeID);
			collapseStatement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	@Override
	public void decollapse(int nodeID)
	{
		try
		{
			decollapseStatement.setInt(1, nodeID);
			decollapseStatement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	@Override
	public void decollapseAll()
	{
		db.execute(decollapseAllStatement);
	}

	@Override
	public List<Integer> getChildren(int nodeID, int layer)
	{
		int nodeLayer = getLayer(nodeID);
		if (layer == nodeLayer)
		{
			// the most common case, for which we have precalculated the virtual
			// children
			return getVirtualChildren(nodeID);
		}
		if (nodeID == getRootID(layer) || nodeLayer >= layer)
		{
			// usually only the case for the root of a partial tree
			List<Integer> frontLine = getChildren(nodeID);
			for (int i = 0; i < frontLine.size();)
			{
				int child = frontLine.get(i);
				if (getLayer(child) > layer)
				{
					frontLine.remove(i);
					frontLine.addAll(i, getChildren(child));
				} else
				{
					i++;
				}
			}
			return frontLine;
		}
		// When we have reached a "cornerstone", pretend it's a leaf:
		return Collections.emptyList();
	}

	private List<Integer> getChildren(int nodeID)
	{
		try
		{
			getRealChildrenStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryIntList(getRealChildrenStatement);
	}

	private List<Integer> getVirtualChildren(int nodeID)
	{
		try
		{
			getVirtualChildrenStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryIntList(getVirtualChildrenStatement);
	}

	@Override
	public String getEdgeLabel(int nodeID)
	{
		try
		{
			getEdgeLabelStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryString(getEdgeLabelStatement, "");
	}

	@Override
	protected void collectLeaves(int nodeID, List<Integer> leaves)
	{
		List<Integer> children = getChildren(nodeID);
		if (children.isEmpty())
		{
			leaves.add(nodeID);
		} else
		{
			for (int child : children)
			{
				collectLeaves(child, leaves);
			}
		}
	}

	@Override
	public String getNodeCaption(int nodeID)
	{
		try
		{
			getNodeCaptionStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryString(getNodeCaptionStatement, "");
	}

	@Override
	public int getNodeStatus(int nodeID)
	{
		try
		{
			getNodeStatusStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryInteger(getNodeStatusStatement, 0);
	}

	@Override
	public int getParent(int nodeID, int layer)
	{
		if (nodeID == getRootID(layer))
		{
			return -1;
		}
		if (getLayer(nodeID) == layer)
		{
			return getVirtualParent(nodeID);
		}
		int result = getParent(nodeID);
		while (getLayer(result) > layer)
		{
			result = getParent(result);
		}
		return result;
	}

	@Override
	public int getRootID()
	{
		return db.queryInteger(getRootStatement, -1);
	}

	@Override
	public int getRootID(int layer)
	{
		if (layer == 0)
		{
			return getRootID();
		}
		int result = getReferenceNode();
		while (getLayer(result) >= layer)
		{
			result = getParent(result);
		}
		return result;
	}

	@Override
	public boolean isCollapsed(int nodeID)
	{
		try
		{
			isCollapsedStatement.setInt(1, nodeID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		int result = db.queryInteger(isCollapsedStatement, 0);
		return result == 1;
	}

	@Override
	public int getSize()
	{
		return db.queryInteger(getSizeStatement, 0);
	}

}
