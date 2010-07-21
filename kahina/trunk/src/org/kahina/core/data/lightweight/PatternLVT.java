package org.kahina.core.data.lightweight;

import java.lang.reflect.Type;
import java.util.regex.Pattern;

import org.kahina.core.data.DataManager;

public class PatternLVT extends AbstractStringLVT<Pattern>
{
	private PatternLVT(LightweightDbStore store, DataManager manager)
	{
		super(store, manager);
	}

	@Override
	protected Pattern unserialize(String serialized)
	{
		return Pattern.compile(serialized);
	}
	
	@Override
	protected String serialize(Pattern value)
	{
		return value.toString();
	}

	public static LVT createPatternLVT(Type type, LightweightDbStore store, DataManager manager)
	{
		if (type == Pattern.class)
		{
			return new PatternLVT(store, manager);
		}
		return null;
	}

}
