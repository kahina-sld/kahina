package org.kahina.core.interfaces;

import org.kahina.core.KahinaException;

import se.sics.jasper.Prolog;
import se.sics.jasper.SICStus;

public class KahinaCallingSicstusInterface implements KahinaPrologInterface
{
	
	private static final boolean VERBOSE = false;
	
	private final Prolog caller;
	
	public KahinaCallingSicstusInterface() throws KahinaInterfaceNotAvailableException
	{		
		caller = SICStus.getCaller();
		
		if (caller == null)
		{
			if (VERBOSE)
			{
				System.err.println("No SICStus Jasper caller available.");
			}
			throw new KahinaInterfaceNotAvailableException();
		}
	}

	@Override
	public void executeQuery(String prologQuery) throws KahinaException
	{
		try
		{
			caller.query(prologQuery, null);
		} catch (Exception e)
		{
			throw new KahinaException("Error in executing Prolog query", e);
		}
	}

}
