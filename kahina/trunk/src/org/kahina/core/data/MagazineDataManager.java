package org.kahina.core.data;

import java.io.File;

public class MagazineDataManager extends MemDataManager
{
	
	private MagazineDataStore defaultStore = new MagazineDataStore();

	@Override
	public void registerDataType(Class<? extends KahinaObject> clazz)
	{
		registerDataType(clazz, defaultStore);
	}

	@Override
	protected void persistSelf(File file)
	{
		// do nothing
	}

	@Override
	protected File getFileForStore(DataStore store, File file)
	{
		if (store == defaultStore)
		{
			return file;
		}
		throw new UnsupportedOperationException("This data manager does not support persistence of self-registered data stores.");
	}
	
	@Override
	public int persistSteps()
	{
		return defaultStore.persistSteps();
	}
	
	@Override
	public void load(File directory)
	{
		defaultStore = MagazineDataStore.load(directory);
	}

}
