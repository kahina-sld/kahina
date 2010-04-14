package org.kahina.core.data.lightweight;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class StringLVT extends LVT
{
	private StringLVT()
	{
	}

	public static StringLVT createStringLVT(Type type)
	{
		if (type == String.class)
		{
			return new StringLVT();
		}
		return null;
	}

	@Override
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store, DataManager manager)
			throws IllegalAccessException
	{
		field.set(object, store.retrieveLongVarchar(objectID, fieldID));
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store, DataManager manager)
			throws IllegalAccessException
	{
		store.storeLongVarchar(objectID, fieldID, (String) field.get(object));
	}

	@Override
	Object retrieveReferenceValue(Integer reference,
			LightweightDbStore store, DataManager manager)
	{
		return store.retrieveReferenceValueLongVarchar(reference);
	}

	@Override
	int storeAsReferenceValue(Object element, LightweightDbStore store,
			DataManager manager)
	{
		return store.storeAsReferenceValueLongVarchar((String) element);
	}

}
