package org.kahina.core.test.data.lightweight;

import gralej.Main;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.kahina.tralesld.data.fs.BracketPacker;
import org.kahina.tralesld.data.fs.StructureSharedString;

public class BracketStructureSharerTest
{

	private BracketPacker sharer;

	@Before
	public void setUp() throws Exception
	{
		sharer = new BracketPacker();
	}

	@Test
	public void test1()
	{
		Scanner scanner = new Scanner(Main.class.getResourceAsStream("/gralej/resource/sample.GRALE"));
		List<String> lines = new ArrayList<String>();
		List<StructureSharedString> results = new ArrayList<StructureSharedString>();
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
		Iterator<StructureSharedString> resultsIterator = results.iterator();
		for (String line : lines)
		{
			Assert.assertEquals(line, resultsIterator.next().toString());
		}
	}

}
