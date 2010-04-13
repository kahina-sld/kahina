package org.kahina.core.data.lightweight;

import java.lang.reflect.Type;

public class StringLVT extends LVT
{
	public static StringLVT createStringLVT(Type type)
	{
		if (type == String.class)
		{
			return new StringLVT();
		}
		return null;
	}

}
