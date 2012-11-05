package org.kahina.core.data;

import java.io.Serializable;

public class KahinaObject implements Serializable
{

	private static final long serialVersionUID = -262730875988653793L;
	
	/**
	 * States whether a view of this object needs to be updated.
	 * @return whether the object was changed since the last call to needsUpdate()
	 */
	public boolean needsUpdate()
	{
	    return true;
	}
}
