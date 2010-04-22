package org.kahina.core.test.data.lightweight;


import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.kahina.core.data.tree.KahinaDbTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.io.database.DatabaseHandler;

public class KahinaDbTreeTest
{
	
	private DatabaseHandler db;
	
	private KahinaTree tree;

	@Before
	public void setUp() throws Exception
	{
		db = new DatabaseHandler(); 
		tree = new KahinaDbTree(db);
	}
	
	@Test
	public void test1()
	{
		int node = tree.addNode("test", "", 2);
		Assert.assertEquals(2, tree.getNodeStatus(node));
	}
	
	@Test
	public void test2()
	{
		int node = tree.addNode("test", "", 0);
		tree.setNodeStatus(node, 2);
		Assert.assertEquals(2, tree.getNodeStatus(node));
	}

	@After
	public void tearDown() throws Exception
	{
		db.close();
	}

}
