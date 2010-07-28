package org.kahina.tralesld.data.fs;

import org.kahina.core.data.KahinaObject;

/**
 * Represents a TraleSLD feature structure, stored in a packed format.
 * @author ke
 *
 */
public abstract class TraleSLDFS extends KahinaObject
{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -4364052716085296822L;

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		toString(builder);
		return builder.toString();
	}
	
	protected abstract void toString(StringBuilder builder);
	
}
