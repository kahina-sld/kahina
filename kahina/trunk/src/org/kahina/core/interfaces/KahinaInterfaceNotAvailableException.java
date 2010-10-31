package org.kahina.core.interfaces;

import org.kahina.core.KahinaException;

public class KahinaInterfaceNotAvailableException extends KahinaException
{
	
	private static final long serialVersionUID = 7628668453459823081L;

	public KahinaInterfaceNotAvailableException()
	{
		this((Throwable) null);
	}
	
	public KahinaInterfaceNotAvailableException(String message)
	{
		this(message, null);
	}

	public KahinaInterfaceNotAvailableException(Throwable cause)
	{
		this("interface not available", cause);
	}
	
	public KahinaInterfaceNotAvailableException(String message, Throwable cause)
	{
		super(message, cause);
	}

}
