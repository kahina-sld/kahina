package org.kahina.tralesld.data.fs;

import gralej.Main;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

import junit.framework.Assert;

import org.junit.Test;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.MemDataManager;
import org.kahina.core.io.database.DatabaseHandler;

public class BracketPackerTest
{
	
	@Test
	public void memTest()
	{
		DataManager dm = new MemDataManager();
		dm.registerDataType(TraleSLDPackedFSNonTerminal.class);
		dm.registerDataType(TraleSLDPackedFSTerminal.class);
		test(dm);
	}
	
	@Test
	public void dbTest()
	{
		DatabaseHandler db = new DatabaseHandler(DatabaseHandler.DatabaseType.DERBY);
		DataManager dm = new DbDataManager(db);
		dm.registerDataType(TraleSLDPackedFSNonTerminal.class);
		dm.registerDataType(TraleSLDPackedFSTerminal.class);
		test(dm);
		db.close();
	}
	
	public void test(DataManager dm)
	{
		TraleSLDFSPacker packer = new TraleSLDFSPacker();
		Scanner scanner = new Scanner(Main.class.getResourceAsStream("/gralej/resource/sample.GRALE"));
		List<String> lines = new ArrayList<String>();
		List<Integer> results = new ArrayList<Integer>();
		while (scanner.hasNextLine())
		{
			packAndStore(scanner.nextLine(), dm, lines, results, packer);
		}
		packAndStore("bla(a)))", dm, lines, results, packer);
		packAndStore("a\"aa", dm, lines, results, packer);
		packAndStore("\"\"", dm, lines, results, packer);
		packAndStore("\"aaaaaaaaa", dm, lines, results, packer);
		packAndStore("(\"))(\"", dm, lines, results, packer);
		packAndStore("(((a)alb", dm, lines, results, packer);
		Iterator<Integer> resultsIterator = results.iterator();
		for (String original : lines)
		{
			String result = dm.retrieve(resultsIterator.next()).toString();
			Assert.assertEquals(original, result);
		}
	}
	
	private void packAndStore(String original, DataManager dm, List<String> originals, List<Integer> packedObjectIDs, TraleSLDFSPacker packer)
	{
		TraleSLDPackedFS fs = packer.pack(original);
		dm.store(fs);
		originals.add(original);
		packedObjectIDs.add(fs.getID());
	}

}
