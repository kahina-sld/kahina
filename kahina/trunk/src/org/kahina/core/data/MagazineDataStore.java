package org.kahina.core.data;

import java.io.File;
import java.io.IOException;

import org.kahina.core.KahinaException;
import org.kahina.core.io.magazine.ObjectMagazine;
import org.kahina.core.util.ProgressMonitorWrapper;

public class MagazineDataStore extends DataStore
{
	
	private ObjectMagazine<KahinaObject> magazine;
	
	public MagazineDataStore()
	{
		try
		{
			File tempFile = File.createTempFile("kahina-magazine", null);
			tempFile.delete();
			magazine = ObjectMagazine.create(tempFile);
		} catch (IOException e)
		{
			throw new KahinaException("I/O error creating magazine data store.", e);
		}
	}

	@Override
	public KahinaObject retrieve(int id)
	{
		return magazine.retrieve(id);
	}

	@Override
	public void store(KahinaObject object, int id)
	{
		magazine.store(id, object);
	}

	public int persistSteps()
	{
		return magazine.persistSteps();
	}
	
	@Override
	public void persist(File file, ProgressMonitorWrapper monitor)
	{
		magazine.persist(file, monitor);
	}

}
