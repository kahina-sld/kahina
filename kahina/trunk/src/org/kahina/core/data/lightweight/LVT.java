package org.kahina.core.data.lightweight;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import org.kahina.core.data.KahinaObject;

/**
 * Represents a lightweight value type.
 * 
 * @author ke
 * 
 */
public abstract class LVT
{
	public static LVT createLVT(Type type)
	{
		LVT result = IntegerLVT.createIntegerLVT(type);
		if (result != null)
		{
			return result;
		}
		result = StringLVT.createStringLVT(type);
		if (result != null)
		{
			return result;
		}
		result = KahinaObjectLVT.createKahinaObjectLVT(type);
		if (result != null)
		{
			return result;
		}
		result = CollectionLVT.createListLVT(type);
		if (result != null)
		{
			return result;
		}
		result = MapLVT.createMapLVT(type);
		return result;
	}

	public static Type[] typeArgumentsForInterface(Type type,
			Class<?> targetRawType)
	{
		Type[] result;
		if (type instanceof ParameterizedType)
		{
			result = typeArgumentsForInterface((ParameterizedType) type,
					targetRawType);
			if (result != null)
			{
				return result;
			}
		}
		if (type instanceof Class<?>)
		{
			Class<?> clazz = (Class<?>) type;
			while (clazz != null)
			{
				for (Type interfaceType : clazz.getGenericInterfaces())
				{
					if (interfaceType instanceof ParameterizedType)
					{
						result = typeArgumentsForInterface(
								(ParameterizedType) interfaceType,
								targetRawType);
						if (result != null)
						{
							return result;
						}
					} else if (interfaceType == targetRawType)
					{
						return new Type[0];
					}
				}
				clazz = (Class<?>) clazz.getGenericSuperclass();
			}
		}
		return null;
	}

	public static Type[] typeArgumentsForInterface(ParameterizedType type,
			Class<?> targetRawType)
	{
		Type rawType = type.getRawType();
		if (rawType == targetRawType)
		{
			return type.getActualTypeArguments();
		}
		return null;
	}

	abstract void retrieveFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store)
			throws IllegalAccessException;

	abstract void storeFieldValue(int objectID, int fieldID, Field field,
			KahinaObject object, LightweightDbStore store)
			throws IllegalAccessException;

}
