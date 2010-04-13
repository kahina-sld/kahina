package org.kahina.core.data.lightweight;

import java.util.List;
import java.util.Set;
import java.lang.reflect.Type;

public class CollectionLVT extends LVT
{
	private LVT elementType;
	
	private boolean set = false;

	public static CollectionLVT createListLVT(Type type)
	{
		CollectionLVT result = new CollectionLVT();
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
		return result;
	}

}
