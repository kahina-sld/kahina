package org.kahina.core.data.lightweight;

import org.kahina.core.data.KahinaObject;

public class KahinaObjectLVT extends LVT
{
	public static LVT createKahinaObjectLVT(Class<?> type)
	{
		if (KahinaObject.class.isAssignableFrom((Class<?>) type))
		{
			return new KahinaObjectLVT();
		}
		return null;
	}
}
