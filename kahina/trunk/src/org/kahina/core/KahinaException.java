package org.kahina.core;

import java.sql.SQLException;

public class KahinaException extends RuntimeException
{
	private static final long serialVersionUID = 1L;
	
	public KahinaException(String message)
	{
		super(message);
	}

	public KahinaException(String message, Exception cause)
	{
		super(message, cause);
	}
}
