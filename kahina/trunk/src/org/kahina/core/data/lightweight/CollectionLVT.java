package org.kahina.core.data.lightweight;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.KahinaException;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class CollectionLVT extends LVT
{
	private LVT elementLVT;

	private Constructor<?> constructor;

	private static Map<DataManager, List<Object>> referenceValuesBeingStoredByManager = new HashMap<DataManager, List<Object>>();

	private List<Object> referenceValuesBeingStored;

	private static Map<DataManager, List<Integer>> referencesBeingStoredByManager = new HashMap<DataManager, List<Integer>>();

	private List<Integer> referencesBeingStored;

	private static Map<DataManager, Map<Integer, Object>> referenceValuesBeingRetrievedByManager = new HashMap<DataManager, Map<Integer, Object>>();

	private Map<Integer, Object> referenceValuesBeingRetrieved;

	private CollectionLVT(LightweightDbStore store, DataManager manager)
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

	public static CollectionLVT createListLVT(Type type,
			LightweightDbStore store, DataManager manager)
	{
		CollectionLVT result = new CollectionLVT(store, manager);

		// Determine element type:
		Type[] arguments = typeArgumentsForInterface(type, List.class);
		if (arguments == null)
		{
			arguments = typeArgumentsForInterface(type, Set.class);
		}
		if (arguments == null)
		{
			return null;
		}
		result.elementLVT = LVT.createLVT((Class<?>) arguments[0], store,
				manager);
		if (result.elementLVT == null)
		{
			return null;
		}

		// Determine constructor:
		if (type instanceof ParameterizedType)
		{
			Type rawType = ((ParameterizedType) type).getRawType();
			if (rawType == List.class)
			{
				try
				{
					result.constructor = ArrayList.class.getConstructor();
				} catch (NoSuchMethodException e)
				{
					throw new KahinaException("Unexpected error.", e);
				}
			} else if (rawType == Set.class)
			{
				try
				{
					result.constructor = HashSet.class.getConstructor();
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
	void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object) throws IllegalAccessException
	{
		field.set(object, retrieveReferenceValue(store.retrieveInt(objectID,
				fieldID)));
	}

	@Override
	Object retrieveReferenceValue(Integer reference)
	{
		if (referenceValuesBeingRetrieved.containsKey(reference))
		{
			return referenceValuesBeingRetrieved.get(reference);
		}
		Collection<Object> collection;
		try
		{
			collection = castToObjectCollection(constructor.newInstance());
		} catch (IllegalArgumentException e)
		{
			throw new KahinaException("Error reconstructing collection.", e);
		} catch (InstantiationException e)
		{
			throw new KahinaException("Error reconstructing collection.", e);
		} catch (IllegalAccessException e)
		{
			throw new KahinaException("Error reconstructing collection.", e);
		} catch (InvocationTargetException e)
		{
			throw new KahinaException("Error reconstructing collection.", e);
		}
		referenceValuesBeingRetrieved.put(reference, collection);
		for (Integer element : store.retrieveCollection(reference))
		{
			collection.add(elementLVT.retrieveReferenceValue(element));
		}
		referenceValuesBeingRetrieved.remove(reference);
		return collection;
	}

	@SuppressWarnings("unchecked")
	private Collection<Object> castToObjectCollection(Object object)
	{
		return (Collection<Object>) object;
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

	@Override
	Integer storeAsReferenceValue(Object object)
	{
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
		for (Object element : castToObjectCollection(object))
		{
			store.storeCollectionElement(reference, elementLVT
					.storeAsReferenceValue(element));
		}
		referencesBeingStored.remove(size);
		referenceValuesBeingStored.remove(size);
		return reference;
	}

	@Override
	void deleteReferenceValue(Integer reference)
	{
		if (elementLVT.deletes())
		{
			List<Integer> elements = store.retrieveCollection(reference);
			for (Integer element : elements)
			{
				elementLVT.deleteReferenceValue(element);
			}
		}
		store.deleteCollection(reference);
	}

	@Override
	boolean deletes()
	{
		return true;
	}
}
