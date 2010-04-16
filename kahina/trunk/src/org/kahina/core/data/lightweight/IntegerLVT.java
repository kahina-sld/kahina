package org.kahina.core.data.lightweight;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class IntegerLVT extends LVT
{
	private IntegerLVT(LightweightDbStore store, DataManager manager)
	{
		super(store, manager);
	}

	public static IntegerLVT createIntegerLVT(Type type,
			LightweightDbStore store, DataManager manager)
	{
		if (type == int.class || type == Integer.class)
		{
			return new IntegerLVT(store, manager);
		}
		return null;
	}

	@Override
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		// works for both int and Integer fields
		field.set(object, store.retrieveInt(objectID, fieldID));
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		// works for both int and Integer fields
		store.storeInt(objectID, fieldID, (Integer) field.get(object));
	}

	@Override
	Object retrieveReferenceValue(Integer reference)
	{
		return reference;
	}

	@Override
	int storeAsReferenceValue(Object element)
	{
		return (Integer) element;
	}

	@Override
	void deleteReferenceValue(Integer reference)
	{
		// integer references refer to nothing but themselves, so do nothing
	}

	@Override
	boolean deletes()
	{
		return false;
	}
}
