package org.kahina.tralesld.data.fs;

import gralej.Main;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

import junit.framework.Assert;

import org.junit.Test;

public class BracketPackerTest
{
	
	@Test
	public void test1()
	{
		TraleSLDFSPacker packer = new TraleSLDFSPacker();
		Scanner scanner = new Scanner(Main.class.getResourceAsStream("/gralej/resource/sample.GRALE"));
		List<String> lines = new ArrayList<String>();
		List<TraleSLDFS> results = new ArrayList<TraleSLDFS>();
		while (scanner.hasNextLine())
		{
			pack(scanner.nextLine(), lines, results, packer);
		}
		pack("bla(a)))", lines, results, packer);
		pack("a\"aa", lines, results, packer);
		pack("\"\"", lines, results, packer);
		pack("\"aaaaaaaaa", lines, results, packer);
		pack("(\"))(\"", lines, results, packer);
		pack("(((a)alb", lines, results, packer);
		Iterator<TraleSLDFS> resultsIterator = results.iterator();
		for (String original : lines)
		{
			String result = resultsIterator.next().toString();
			Assert.assertEquals(original, result);
		}
	}
	
	private void pack(String original, List<String> originals, List<TraleSLDFS> fss, TraleSLDFSPacker packer)
	{
		TraleSLDFS fs = packer.pack(original);
		originals.add(original);
		fss.add(fs);
	}

}
