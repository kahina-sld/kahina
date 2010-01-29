package org.kahina.core.data;



public abstract class DataManager
{
	protected abstract DataStore getStoreForClass(Class<? extends KahinaObject> clazz);

	public abstract void registerDataType(Class<? extends KahinaObject> clazz, DataStore store);

	public abstract void registerDataType(Class<? extends KahinaObject> clazz);

	public void store(KahinaObject object)
	{
		getStoreForClass(object.getClass()).store(object);
	}

	public KahinaObject retrieve(Class<KahinaObject> clazz, int id)
	{
		return getStoreForClass(clazz).retrieve(id);
	}
}