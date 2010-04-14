package org.kahina.core.data.lightweight;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class IntegerLVT extends LVT
{
	public static IntegerLVT createIntegerLVT(Type type)
	{
		if (type == int.class || type == Integer.class)
		{
			return new IntegerLVT();
		}
		return null;
	}

	@Override
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store, DataManager manager)
			throws IllegalAccessException
	{
		// works for both int and Integer fields
		field.set(object, store.retrieveInt(objectID, fieldID));
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store, DataManager manager)
			throws IllegalAccessException
	{
		// works for both int and Integer fields
		store.storeInt(objectID, fieldID, (Integer) field.get(object));
	}

	@Override
	public Object retrieveReferenceValue(Integer reference,
			LightweightDbStore store, DataManager manager)
	{
		return reference;
	}

	@Override
	public int storeAsReferenceValue(Object element, LightweightDbStore store,
			DataManager manager)
	{
		return (Integer) element;
	}
}
