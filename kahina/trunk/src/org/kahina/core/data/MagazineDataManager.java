package org.kahina.core.data;

public class MagazineDataManager extends MemDataManager
{
	
	private DataStore defaultStore = new MagazineDataStore();

	@Override
	public void registerDataType(Class<? extends KahinaObject> clazz)
	{
		registerDataType(clazz, defaultStore);
	}

}
