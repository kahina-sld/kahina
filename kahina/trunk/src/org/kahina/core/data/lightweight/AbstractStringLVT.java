package org.kahina.core.data.lightweight;

import java.lang.reflect.Field;

import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public abstract class AbstractStringLVT<T> extends LVT
{

	public AbstractStringLVT(LightweightDbStore store, DataManager manager)
	{
		super(store, manager);
	}

	protected abstract String serialize(T value);

	protected abstract T unserialize(String serialized);

	@Override
	protected void deleteReferenceValue(Integer reference)
	{
		store.deleteLongVarchar(reference);
	}

	@Override
	protected boolean deletes()
	{
		return true;
	}

	@Override
	protected void retrieveFieldValue(int objectID, int fieldID, Field field, KahinaObject object) throws IllegalAccessException
	{
		field.set(object, unserialize(store.retrieveLongVarchar(objectID, fieldID)));
	}

	@Override
	protected Object retrieveReferenceValue(Integer reference)
	{
		return unserialize(store.retrieveReferenceValueLongVarchar(reference));
	}

	@Override
	protected Integer storeAsReferenceValue(Object object)
	{
		return store.storeAsReferenceValueLongVarchar(serialize(castToType(object)));
	}

	@Override
	protected void storeFieldValue(int objectID, int fieldID, Field field, KahinaObject object) throws IllegalAccessException
	{
		store.storeLongVarchar(objectID, fieldID, serialize(castToType(field.get(object))));
	}

	@SuppressWarnings("unchecked")
	private T castToType(Object object)
	{
		return (T) object;
	}

}