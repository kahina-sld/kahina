package org.kahina.tralesld.data.fs;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

/**
 * Represents a TraleSLD feature structure, stored in a packed format.
 * @author ke
 *
 */
public abstract class TraleSLDFS extends KahinaObject implements LightweightKahinaObject
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
