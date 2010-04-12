package org.kahina.core.data.lightweight;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * Represents a lightweight value type.
 * 
 * @author ke
 * 
 */
public abstract class LVT
{

	/**
	 * 
	 * @param type
	 *            Where possible, this should be retrieved by reflection methods
	 *            that can return <b>parameterized</b> types (and cast to
	 *            {@code Class<?>}, obviously).
	 * @return
	 */
	public static LVT createLVT(Class<?> type)
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
		result = ListLVT.createListLVT(type);
		if (result != null)
		{
			return result;
		}
		result = SetLVT.createSetLVT(type);
		if (result != null)
		{
			return result;
		}
		result = MapLVT.createMapLVT(type);
		return result;
	}

	public static Type[] typeArgumentsForInterface(Class<?> type,
			Class<?> targetRawType)
	{
		while (type != null)
		{
			for (Type interfaceType : type.getGenericInterfaces())
			{
				if (interfaceType instanceof ParameterizedType)
				{
					ParameterizedType parameterizedInterfaceType = (ParameterizedType) interfaceType;
					if (parameterizedInterfaceType.getRawType() == targetRawType)
					{
						return parameterizedInterfaceType
								.getActualTypeArguments();
					}
				} else if (interfaceType == targetRawType)
				{
					return new Type[0];
				}
			}
			type = (Class<?>) type.getGenericSuperclass();
		}
		return null;
	}

}
