package org.kahina.core.interfaces;

import org.kahina.core.KahinaException;

import se.sics.jasper.Jasper;
import se.sics.jasper.Prolog;

public class KahinaCallingSicstusInterface implements KahinaPrologInterface
{
	
	private static final boolean VERBOSE = false;
	
	private final Prolog caller;
	
	public KahinaCallingSicstusInterface() throws KahinaInterfaceNotAvailableException
	{
		try
		{
			Jasper.newProlog().query("true.", null);
		} catch (Exception e)
		{
			throw new KahinaInterfaceNotAvailableException(e);
		}
		
		caller = Jasper.getCaller();
		
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
