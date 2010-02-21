package org.kahina.data.tree;

import java.util.List;

import org.kahina.io.database.DatabaseHandler;

public class KahinaDbTree extends KahinaTree
{
	private static final String CLIENT_ID = KahinaDbTree.class.getName();

	private static final String TABLE_NAME_PREFIX = KahinaDbTree.class
			.getSimpleName()
			+ "_";

	private static final String NODE_TABLE_NAME = TABLE_NAME_PREFIX + "nodes";

	private static final String EDGE_TABLE_NAME = TABLE_NAME_PREFIX + "edges";

	private DatabaseHandler db;
	
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
	}

	private void createTablesIfNecessary()
	{
		if (!db.isRegistered(CLIENT_ID))
		{
			db
					.execute("CREATE TABLE "
							+ NODE_TABLE_NAME
							+ " (id INT, nodeCaption LONG VARCHAR, edgeLabel LONG VARCHAR, collapsed TINYINT(1), terminal TINYINT(1), PRIMARY KEY id)");
			db
					.execute("CREATE TABLE "
							+ EDGE_TABLE_NAME
							+ " (child_id INT, parent_id INT, layer INT, INDEX child_id (child_id), INDEX parent_id (parent_id), INDEX layer (layer))");
		}
	}
	
	private void prepareStatements()
	{
		// TODO
	}

	@Override
	public void addChild(int parent, int child)
	{
		// add
		// nach oben gehen und jedem
	}

	@Override
	public int addNode(String caption, String label, int nodeStatus)
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void clear()
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void collapse(int nodeID)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void decollapse(int nodeID)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void decollapseAll()
	{
		// TODO Auto-generated method stub

	}

	@Override
	public List<Integer> getChildren(int nodeID, int layerID)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getEdgeLabel(int nodeID)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Integer> getLeaves()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getNodeCaption(int nodeID)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getNodeStatus(int nodeID)
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getParent(int nodeID, int layerID)
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getRootID()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getRootID(int layerID)
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean hasCollapsedAncestor(int nodeID)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isCollapsed(int nodeID)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setPrimaryModel(KahinaTree primaryModel)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void setRootID(int rootID)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void toggleCollapse(int nodeID)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public int getSize()
	{
		// TODO Auto-generated method stub
		return 0;
	}

}
