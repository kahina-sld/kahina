package org.kahina.core.test.data.lightweight;

import gralej.Main;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.kahina.tralesld.data.fs.TraleSLDFSPacker;
import org.kahina.tralesld.data.fs.TraleSLDPackedFS;

public class BracketPackerTest
{

	private TraleSLDFSPacker sharer;

	@Before
	public void setUp() throws Exception
	{
		sharer = new TraleSLDFSPacker();
	}

	@Test
	public void test1()
	{
		Scanner scanner = new Scanner(Main.class.getResourceAsStream("/gralej/resource/sample.GRALE"));
		List<String> lines = new ArrayList<String>();
		List<TraleSLDPackedFS> results = new ArrayList<TraleSLDPackedFS>();
		while (scanner.hasNextLine())
		{
			String line = scanner.nextLine();
			lines.add(line);
			results.add(sharer.pack(line));
		}
		String unbalanced = "bla(a)))";
		lines.add(unbalanced);
		results.add(sharer.pack(unbalanced));
		unbalanced = "(((a)alb";
		lines.add(unbalanced);
		results.add(sharer.pack(unbalanced));
		unbalanced = "a\"aa";
		lines.add(unbalanced);
		results.add(sharer.pack(unbalanced));
		unbalanced = "\"\"";
		lines.add(unbalanced);
		results.add(sharer.pack(unbalanced));
		unbalanced = "\"aaaaaaaaa";
		lines.add(unbalanced);
		results.add(sharer.pack(unbalanced));
		unbalanced = "(\"))(\"";
		lines.add(unbalanced);
		results.add(sharer.pack(unbalanced));
		Iterator<TraleSLDPackedFS> resultsIterator = results.iterator();
		for (String line : lines)
		{
			Assert.assertEquals(line, resultsIterator.next().toString());
		}
	}

}
