package org.kahina.core.data.lightweight;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class KahinaObjectLVT extends LVT
{
	public static LVT createKahinaObjectLVT(Type type)
	{
		if (type instanceof Class<?>
				&& KahinaObject.class.isAssignableFrom((Class<?>) type))
		{
			return new KahinaObjectLVT();
		}
		return null;
	}

	@Override
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store, DataManager manager)
			throws IllegalAccessException
	{
		field.set(object, manager.retrieve(store.retrieveInt(objectID, fieldID)));
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store)
			throws IllegalAccessException
	{
		// TODO
	}
}
