package org.kahina.core.interfaces;

import org.kahina.core.KahinaException;

import se.sics.jasper.Prolog;
import se.sics.jasper.SICStus;

public class KahinaCallingSicstusInterface implements KahinaPrologInterface
{

	private static final boolean VERBOSE = false;

	private final Prolog prolog;

	public KahinaCallingSicstusInterface() throws KahinaInterfaceNotAvailableException
	{
		try
		{
			prolog = SICStus.getCaller().newProlog();
		} catch (InterruptedException e)
		{
			throw new KahinaInterfaceNotAvailableException(e);
		}

		if (prolog == null)
		{
			if (VERBOSE)
			{
				System.err.println("No SICStus Jasper caller available.");
			}
			throw new KahinaInterfaceNotAvailableException();
		}
	}

	@Override
	public void executeQuery(final String prologQuery) throws KahinaException
	{
		try
		{
			prolog.query(prologQuery, null);
		} catch (Exception e)
		{
			throw new KahinaException("Error in executing Prolog query", e);
		}
	}

}
