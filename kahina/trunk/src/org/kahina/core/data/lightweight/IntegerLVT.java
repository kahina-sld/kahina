package org.kahina.core.data.lightweight;

public class IntegerLVT extends LVT
{
	public static IntegerLVT createIntegerLVT(Class<?> type)
	{
		if (type == int.class)
		{
			return new IntegerLVT();
		}
		return null;
	}
}
