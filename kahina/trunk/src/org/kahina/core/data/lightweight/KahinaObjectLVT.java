package org.kahina.core.data.lightweight;

import java.lang.reflect.Type;

import org.kahina.core.data.KahinaObject;

public class KahinaObjectLVT extends LVT
{
	public static LVT createKahinaObjectLVT(Type type)
	{
		if (type instanceof Class<?>
				&& KahinaObject.class.isAssignableFrom((Class<?>) type))
		{
			return new KahinaObjectLVT();
		}
		return null;
	}
}
