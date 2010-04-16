package org.kahina.core.test.data.lightweight;


import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.fs.KahinaFeatureStructure;
import org.kahina.core.io.database.DatabaseHandler;
import org.kahina.tralesld.TraleSLDStep;

public class LightweightDbStoreTest
{

	private DataManager manager;
	
	private DatabaseHandler db;

	@Before
	public void setUp() throws Exception
	{
		db = new DatabaseHandler();
		manager = new DbDataManager(db);
		manager.registerDataType(KahinaFeatureStructure.class);
		manager.registerDataType(TraleSLDStep.class);
	}
	
	@Test
	public void storeAndRetrieveKahinaFS()
	{
		KahinaFeatureStructure fs = new KahinaFeatureStructure();
		fs.grisuMessage = "Hallo Welt!";
		manager.store(fs);
		fs = manager.retrieve(KahinaFeatureStructure.class, fs.getID());
		Assert.assertEquals("Hallo Welt!", fs.grisuMessage);
	}
	
	@Test
	public void storeAndRetrieveTraleSLDStep()
	{
		TraleSLDStep step = new TraleSLDStep();
		manager.store(step);
		manager.retrieve(step.getID());
	}

	@After
	public void tearDown() throws Exception
	{
		db.close();
	}

}
