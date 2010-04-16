package org.kahina.core.data.lightweight;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class KahinaObjectLVT extends LVT
{
	public KahinaObjectLVT(LightweightDbStore store, DataManager manager)
	{
		super(store, manager);
	}

	public static LVT createKahinaObjectLVT(Type type,
			LightweightDbStore store, DataManager manager)
	{
		if (type instanceof Class<?>
				&& KahinaObject.class.isAssignableFrom((Class<?>) type))
		{
			return new KahinaObjectLVT(store, manager);
		}
		return null;
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		Integer reference = storeAsReferenceValue(field.get(object));
		store.storeInt(objectID, fieldID, reference);
	}

	@Override
	Integer storeAsReferenceValue(Object value)
	{
		if (value == null)
		{
			return null;
		}
		KahinaObject object = (KahinaObject) value;
		manager.store(object);
		return object.getID();
	}

	@Override
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		Integer reference = store.retrieveInt(objectID, fieldID);
		field.set(object, retrieveReferenceValue(reference));
	}

	@Override
	Object retrieveReferenceValue(Integer reference)
	{
		if (reference == null)
		{
			return null;
		}
		return manager.retrieve(reference);
	}

	@Override
	void deleteReferenceValue(Integer reference)
	{
		// KahinaObjects cannot be deleted for now, so do nothing
	}

	@Override
	boolean deletes()
	{
		return false;
	}
}
