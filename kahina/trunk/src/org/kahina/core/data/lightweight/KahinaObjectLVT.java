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

	public static LVT createKahinaObjectLVT(Type type, LightweightDbStore store, DataManager manager)
	{
		if (type instanceof Class<?>
				&& KahinaObject.class.isAssignableFrom((Class<?>) type))
		{
			return new KahinaObjectLVT(store, manager);
		}
		return null;
	}

	@Override
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object)
			throws IllegalAccessException
	{
		field.set(object, manager
				.retrieve(store.retrieveInt(objectID, fieldID)));
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object)
			throws IllegalAccessException
	{
		store.storeInt(objectID, fieldID, object.getID());
		manager.store((KahinaObject) field.get(object));
	}

	@Override
	Object retrieveReferenceValue(Integer reference)
	{
		return manager.retrieve(reference);
	}

	@Override
	int storeAsReferenceValue(Object value)
	{
		KahinaObject object = (KahinaObject) value;
		manager.store(object);
		return object.getID();
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
