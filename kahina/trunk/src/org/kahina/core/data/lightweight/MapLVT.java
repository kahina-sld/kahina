package org.kahina.core.data.lightweight;

import java.lang.reflect.Type;
import java.util.Map;

public class MapLVT extends LVT
{
	private LVT keyType;
	
	private LVT valueType;

	public static LVT createMapLVT(Class<?> type)
	{
		Type[] arguments = typeArgumentsForInterface(type, Map.class);
		if (arguments == null)
		{
			return null;
		}
		MapLVT result = new MapLVT();
		result.keyType = LVT.createLVT((Class<?>) arguments[0]);
		result.valueType = LVT.createLVT((Class<?>) arguments[1]);
		return result;
	}

}
