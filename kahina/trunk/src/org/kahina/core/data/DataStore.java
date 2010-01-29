package org.kahina.core.data;



public interface DataStore
{
	public void store(KahinaObject object, int id);

	public int store(KahinaObject object);

	public KahinaObject retrieve(int id);
}