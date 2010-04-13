package org.kahina.core.data.lightweight;

import java.lang.reflect.Type;
import java.util.Set;

public class SetLVT extends LVT
{
	private LVT elementType;
	
	public static SetLVT createSetLVT(Class<?> type)
	{
		Type[] arguments = typeArgumentsForInterface(type, Set.class);
		if (arguments == null)
		{
			return null;
		}
		SetLVT result = new SetLVT();
		result.elementType = LVT.createLVT((Class<?>) arguments[0]);
		if (result.elementType == null)
		{
			return null;
		}
		return result;
	}
}
