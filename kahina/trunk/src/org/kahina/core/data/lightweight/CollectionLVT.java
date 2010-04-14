package org.kahina.core.data.lightweight;

import java.lang.reflect.Constructor;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kahina.core.KahinaException;

public class CollectionLVT extends LVT
{
	private LVT elementType;

	private boolean set = false;

	private Constructor constructor;

	public static CollectionLVT createListLVT(Type type)
	{
		CollectionLVT result = new CollectionLVT();

		// Determine element type:
		Type[] arguments = typeArgumentsForInterface(type, List.class);
		if (arguments == null)
		{
			arguments = typeArgumentsForInterface(type, Set.class);
			result.set = true;
		}
		if (arguments == null)
		{
			return null;
		}
		result.elementType = LVT.createLVT((Class<?>) arguments[0]);
		if (result.elementType == null)
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

}
