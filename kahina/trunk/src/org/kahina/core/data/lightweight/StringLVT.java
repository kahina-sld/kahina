package org.kahina.core.data.lightweight;

public class StringLVT extends LVT
{
	public static StringLVT createStringLVT(Class<?> type)
	{
		if (type == String.class)
		{
			return new StringLVT();
		}
		return null;
	}

}
