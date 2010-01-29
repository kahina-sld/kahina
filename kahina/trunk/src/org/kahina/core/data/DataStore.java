package org.kahina.core.data;



public interface DataStore
{
	public void store(KahinaObject object);
	
	public KahinaObject retrieve(int id);
}