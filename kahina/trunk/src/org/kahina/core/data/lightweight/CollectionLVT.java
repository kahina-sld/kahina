package org.kahina.core.data.lightweight;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kahina.core.KahinaException;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class CollectionLVT extends LVT
{
	private LVT elementLVT;

	private Constructor<?> constructor;

	public static CollectionLVT createListLVT(Type type)
	{
		CollectionLVT result = new CollectionLVT();

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
		result.elementLVT = LVT.createLVT((Class<?>) arguments[0]);
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
			KahinaObject object, LightweightDbStore store, DataManager manager)
			throws IllegalAccessException
	{
		field.set(object, retrieveReferenceValue(store.retrieveInt(objectID,
				fieldID), store, manager));
	}

	@Override
	Object retrieveReferenceValue(Integer reference,
			LightweightDbStore store, DataManager manager)
	{
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
		for (Integer element : store.retrieveCollection(reference))
		{
			collection.add(elementLVT.retrieveReferenceValue(element, store,
					manager));
		}
		return collection;
	}

	@SuppressWarnings("unchecked")
	private Collection<Object> castToObjectCollection(Object object)
	{
		return (Collection<Object>) object;
	}

	@Override
	void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store, DataManager manager)
			throws IllegalAccessException
	{
		for (Object element : castToObjectCollection(field.get(object)))
		{
			int reference = elementLVT.storeAsReferenceValue(element, store,
					manager);
			store.storeInt(objectID, fieldID, reference);
		}
	}

}
