package org.kahina.core;


public class KahinaException extends RuntimeException
{
	private static final long serialVersionUID = 1L;
	
	public KahinaException(String message)
	{
		super(message);
	}

	public KahinaException(String message, Throwable cause)
	{
		super(message, cause);
	}
}
