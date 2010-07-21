package org.kahina.core.data.lightweight;

import java.lang.reflect.Type;

import org.kahina.core.data.DataManager;

public class StringLVT extends AbstractStringLVT<String>
{
	private StringLVT(LightweightDbStore store, DataManager manager)
	{
		super(store, manager);
	}

	public static StringLVT createStringLVT(Type type,
			LightweightDbStore store, DataManager manager)
	{
		if (type == String.class)
		{
			return new StringLVT(store, manager);
		}
		return null;
	}

	@Override
	protected String serialize(String value)
	{
		return value;
	}

	@Override
	protected String unserialize(String serialized)
	{
		return serialized;
	}

}
