package org.kahina.core.test.data.lightweight;

import java.util.ArrayList;
import java.util.List;

import junit.framework.Assert;
import junit.framework.AssertionFailedError;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.io.database.DatabaseHandler;
import org.kahina.tralesld.TraleSLDStep;
import org.kahina.tralesld.data.fs.TraleSLDFSPacker;
import org.kahina.tralesld.data.fs.TraleSLDPackedFS;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSNonTerminal;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSTerminal;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;

public class LightweightDbStoreTest
{

	private DataManager manager;
	
	private DatabaseHandler db;
	
	private TraleSLDFSPacker packer;

	@Before
	public void setUp() throws Exception
	{
		db = new DatabaseHandler(DatabaseHandler.DatabaseType.DERBY);
		manager = new DbDataManager(db);
		manager.registerDataType(TraleSLDPackedFSTerminal.class);
		manager.registerDataType(TraleSLDPackedFSNonTerminal.class);
		manager.registerDataType(TraleSLDVariableBinding.class);
		manager.registerDataType(TraleSLDVariableBindingSet.class);
		manager.registerDataType(TraleSLDStep.class);
		manager.registerDataType(TestKahinaObject.class);
		packer = new TraleSLDFSPacker();
	}
	
	@Test
	public void storeAndRetrieveKahinaFS()
	{
		TraleSLDPackedFS fs = packer.pack("Hallo Welt!");
		manager.store(fs);
		fs = manager.retrieve(TraleSLDPackedFS.class, fs.getID());
		Assert.assertEquals("Hallo Welt!", fs.toString());
	}
	
	@Test
	public void storeAndRetrieveTraleSLDStep()
	{
		TraleSLDStep step = new TraleSLDStep();
		manager.store(step);
		manager.retrieve(step.getID());
	}
	
	@Test
	public void storeAndRetrieveIntegerList()
	{
		List<Integer> list = new ArrayList<Integer>();
		list.add(4);
		list.add(7);
		list.add(9);
		TestKahinaObject object = new TestKahinaObject();
		object.integers = list;
		manager.store(object);
		object = manager.retrieve(TestKahinaObject.class, object.getID());
		Assert.assertEquals(list, object.integers);
	}
	
	@Test
	public void storeAndRetrieveTestObjectList()
	{
		List<TestKahinaObject> list = new ArrayList<TestKahinaObject>();
		list.add(new TestKahinaObject(4));
		list.add(new TestKahinaObject(7));
		list.add(new TestKahinaObject(9));
		TestKahinaObject object = new TestKahinaObject(11);
		object.objects = list;
		manager.store(object);
		object = manager.retrieve(TestKahinaObject.class, object.getID());
		try
		{
			Assert.assertEquals(list, object.objects);
		} catch (AssertionFailedError e)
		{
			System.err.println("Expected: " + list);
			System.err.println("Actual: " + object.objects);
			throw e;
		}
	}
	
	@Test
	public void storeAndRetrieveIntListList()
	{
		List<Integer> a = new ArrayList<Integer>();
		List<Integer> b = new ArrayList<Integer>();
		List<Integer> c = new ArrayList<Integer>();
		List<Integer> d = null;
		a.add(4);
		a.add(5);
		b.add(6);
		List<List<Integer>> list = new ArrayList<List<Integer>>();
		list.add(a);
		list.add(b);
		list.add(c);
		list.add(d);
		TestKahinaObject object = new TestKahinaObject();
		object.intLists = list;
		manager.store(object);
		object = manager.retrieve(TestKahinaObject.class, object.getID());
		Assert.assertEquals(list, object.intLists);
	}
	
	@Test
	public void storeAndRetrieveIndirectlyCyclicList()
	{
		List<TestKahinaObject> list = new ArrayList<TestKahinaObject>();
		TestKahinaObject object = new TestKahinaObject();
		object.objects = list;
		list.add(object);
		manager.store(object);
		object = manager.retrieve(TestKahinaObject.class, object.getID());
		Assert.assertEquals(list, object.objects);
	}

	@After
	public void tearDown() throws Exception
	{
		db.close();
	}

}
