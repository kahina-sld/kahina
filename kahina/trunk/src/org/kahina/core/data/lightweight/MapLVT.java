package org.kahina.core.data.lightweight;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.KahinaException;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class MapLVT extends LVT
{
	private LVT keyLVT;

	private LVT valueLVT;

	private Constructor<?> constructor;

	// TODO think scope over - allow multiple references to the same structure
	// within one field?

	private static Map<DataManager, List<Object>> referenceValuesBeingStoredByManager = new HashMap<DataManager, List<Object>>();

	private List<Object> referenceValuesBeingStored;

	private static Map<DataManager, List<Integer>> referencesBeingStoredByManager = new HashMap<DataManager, List<Integer>>();

	private List<Integer> referencesBeingStored;

	private static Map<DataManager, Map<Integer, Object>> referenceValuesBeingRetrievedByManager = new HashMap<DataManager, Map<Integer, Object>>();

	private Map<Integer, Object> referenceValuesBeingRetrieved;

	protected MapLVT(LightweightDbStore store, DataManager manager)
	{
		super(store, manager);
		if (!referenceValuesBeingStoredByManager.containsKey(store))
		{
			referenceValuesBeingStoredByManager.put(manager,
					new ArrayList<Object>());
			referenceValuesBeingRetrievedByManager.put(manager,
					new HashMap<Integer, Object>());
			referencesBeingStoredByManager.put(manager,
					new ArrayList<Integer>());
		}
		referenceValuesBeingStored = referenceValuesBeingStoredByManager
				.get(manager);
		referencesBeingStored = referencesBeingStoredByManager.get(manager);
		referenceValuesBeingRetrieved = referenceValuesBeingRetrievedByManager
				.get(manager);
	}

	public static LVT createMapLVT(Type type, LightweightDbStore store,
			DataManager manager)
	{
		Type[] arguments = typeArgumentsForInterface(type, Map.class);

		// Determine key/value types:
		if (arguments == null)
		{
			return null;
		}
		MapLVT result = new MapLVT(store, manager);
		result.keyLVT = LVT.createLVT(arguments[0], store, manager);
		// Only integers and objects are supported as keys:
		if (!(result.keyLVT instanceof IntegerLVT || result.keyLVT instanceof KahinaObjectLVT))
		{
			return null;
		}
		result.valueLVT = LVT.createLVT(arguments[1], store, manager);
		if (result.valueLVT == null)
		{
			return null;
		}

		// Determine constructor:
		if (type instanceof ParameterizedType)
		{
			if (((ParameterizedType) type).getRawType() == Map.class)
			{
				try
				{
					result.constructor = HashMap.class.getConstructor();
				} catch (NoSuchMethodException e)
				{
					throw new KahinaException("Unexpected error.", e);
				}
			}
		}
		if (result.constructor == null)
		{
			Class<?> instantiationCandidate;
			if (type instanceof Class<?>)
			{
				instantiationCandidate = (Class<?>) type;
			} else if (type instanceof ParameterizedType)
			{
				// TODO is the cast to Class<?> safe?
				instantiationCandidate = (Class<?>) ((ParameterizedType) type)
						.getRawType();
			} else
			{
				return null;
			}
			try
			{
				result.constructor = instantiationCandidate.getConstructor();
			} catch (NoSuchMethodException e)
			{
				return null;
			}
		}

		return result;
	}

	@Override
	void deleteReferenceValue(Integer reference)
	{
		if (valueLVT.deletes())
		{
			List<Integer> values = store.retrieveMapValues(reference);
			for (Integer value : values)
			{
				valueLVT.deleteReferenceValue(value);
			}
		}
		store.deleteMap(reference);
	}

	@Override
	boolean deletes()
	{
		return true;
	}

	@Override
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		field.set(object, retrieveReferenceValue(store.retrieveInt(objectID,
				fieldID)));
	}

	@Override
	Object retrieveReferenceValue(Integer reference)
	{
		if (reference == null)
		{
			return null;
		}
		if (referenceValuesBeingRetrieved.containsKey(reference))
		{
			return referenceValuesBeingRetrieved.get(reference);
		}
		Map<Object, Object> map;
		try
		{
			map = castToObjectObjectMap(constructor.newInstance());
		} catch (IllegalArgumentException e)
		{
			throw new KahinaException("Error reconstructing map.", e);
		} catch (InstantiationException e)
		{
			throw new KahinaException("Error reconstructing map.", e);
		} catch (IllegalAccessException e)
		{
			throw new KahinaException("Error reconstructing map.", e);
		} catch (InvocationTargetException e)
		{
			throw new KahinaException("Error reconstructing map.", e);
		}
		referenceValuesBeingRetrieved.put(reference, map);
		for (Map.Entry<Integer, Integer> entry : store.retrieveMap(reference)
				.entrySet())
		{
			map.put(keyLVT.retrieveReferenceValue(entry.getKey()), valueLVT
					.retrieveReferenceValue(entry.getValue()));
		}
		referenceValuesBeingRetrieved.remove(reference);
		return map;
	}

	@SuppressWarnings("unchecked")
	private Map<Object, Object> castToObjectObjectMap(Object object)
	{
		return (Map<Object, Object>) object;
	}

	@Override
	Integer storeAsReferenceValue(Object object)
	{
		if (object == null)
		{
			return null;
		}
		int size = referenceValuesBeingStored.size();
		for (int i = 0; i < size; i++)
		{
			if (referenceValuesBeingStored.get(i) == object)
			{
				return referencesBeingStored.get(i);
			}
		}
		int reference = store.getNewCollectionReference();
		referenceValuesBeingStored.add(object);
		referencesBeingStored.add(reference);
		for (Map.Entry<Object, Object> entry : castToObjectObjectMap(object).entrySet())
		{
			Integer keyReference = keyLVT.storeAsReferenceValue(entry.getKey());
			Integer valueReference = valueLVT.storeAsReferenceValue(entry.getValue());
			store.storeMapEntry(reference, keyReference, valueReference);
		}
		referencesBeingStored.remove(size);
		referenceValuesBeingStored.remove(size);
		return reference;
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		Integer oldReference = store.retrieveInt(objectID, fieldID);
		if (oldReference != null)
		{
			deleteReferenceValue(oldReference);
		}
		store.storeInt(objectID, fieldID, storeAsReferenceValue(field
				.get(object)));
	}
}
