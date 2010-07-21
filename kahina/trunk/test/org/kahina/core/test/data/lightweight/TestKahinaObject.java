package org.kahina.core.test.data.lightweight;

import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class TestKahinaObject extends KahinaObject implements
		LightweightKahinaObject
{
	
	public int myID;
	
	public List<Integer> integers;
	
	public List<TestKahinaObject> objects;
	
	public List<List<Integer>> intLists;
	
	public Map<Integer, Integer> integerByInteger;

	public Map<Integer, List<Integer>> integersByInteger;

	public Map<List<Integer>, String> stringByIntegers;
	
	public Pattern pattern;
	
	public TestKahinaObject()
	{
	}
	
	public TestKahinaObject(int myID)
	{
		this.myID = myID;
	}
	
	public boolean equals(Object o)
	{
		if (!(o instanceof TestKahinaObject))
		{
			return false;
		}
		
		return ((TestKahinaObject) o).myID == myID;
	}
	
	public int hashCode()
	{
		return myID;
	}

}
