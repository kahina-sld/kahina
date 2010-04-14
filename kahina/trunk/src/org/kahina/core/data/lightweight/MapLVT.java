package org.kahina.core.data.lightweight;

import java.lang.reflect.Constructor;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

import org.kahina.core.KahinaException;

public class MapLVT extends LVT
{
	private LVT keyType;
	
	private LVT valueType;
	
	private Constructor<?> constructor;

	public static LVT createMapLVT(Type type)
	{
		Type[] arguments = typeArgumentsForInterface(type, Map.class);
		
		// Determine key/value types:
		if (arguments == null)
		{
			return null;
		}
		MapLVT result = new MapLVT();
		result.keyType = LVT.createLVT((Class<?>) arguments[0]);
		if (result.keyType == null)
		{
			return null;
		}
		result.valueType = LVT.createLVT((Class<?>) arguments[1]);
		if (result.valueType == null)
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

}
