package org.kahina.core.data.lightweight;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class StringLVT extends LVT
{
	private StringLVT(LightweightDbStore store, DataManager manager)
	{
		super(store, manager);
	}

	public static StringLVT createStringLVT(Type type,
			LightweightDbStore store, DataManager manager)
	{
		if (type == String.class)
		{
			return new StringLVT(store, manager);
		}
		return null;
	}

	@Override
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		field.set(object, store.retrieveLongVarchar(objectID, fieldID));
	}

	@Override
	Object retrieveReferenceValue(Integer reference)
	{
		return store.retrieveReferenceValueLongVarchar(reference);
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		store.storeLongVarchar(objectID, fieldID, (String) field.get(object));
	}

	@Override
	Integer storeAsReferenceValue(Object element)
	{
		return store.storeAsReferenceValueLongVarchar((String) element);
	}

	@Override
	void deleteReferenceValue(Integer reference)
	{
		store.deleteLongVarchar(reference);
	}

	@Override
	boolean deletes()
	{
		return true;
	}

}
