package org.kahina.core.data.lightweight;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

import org.kahina.core.KahinaException;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.KahinaObject;

public class MapLVT extends LVT
{
	private LVT keyLVT;
	
	private LVT valueLVT;
	
	private Constructor<?> constructor;
	
	protected MapLVT(LightweightDbStore store, DataManager manager)
	{
		super(store, manager);
	}

	public static LVT createMapLVT(Type type, LightweightDbStore store, DataManager manager)
	{
		Type[] arguments = typeArgumentsForInterface(type, Map.class);
		
		// Determine key/value types:
		if (arguments == null)
		{
			return null;
		}
		MapLVT result = new MapLVT(store, manager);
		result.keyLVT = LVT.createLVT((Class<?>) arguments[0], store, manager);
		if (result.keyLVT == null)
		{
			return null;
		}
		result.valueLVT = LVT.createLVT((Class<?>) arguments[1], store, manager);
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
}
