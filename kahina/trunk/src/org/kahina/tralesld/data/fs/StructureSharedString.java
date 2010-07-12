package org.kahina.tralesld.data.fs;

import org.kahina.core.data.KahinaObject;

public abstract class StructureSharedString extends KahinaObject
{
	
	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		toString(builder);
		return builder.toString();
	}
	
	protected abstract void toString(StringBuilder builder);
	
}
