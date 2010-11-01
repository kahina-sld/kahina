package org.kahina.core.interfaces;

public class KahinaPrologInterfaceFactory
{
	
	private static final boolean VERBOSE = false;
	
	public static KahinaPrologInterface create()
	{
		try {
			return new KahinaCallingSicstusInterface();
		} catch (KahinaInterfaceNotAvailableException e)
		{
			// just continue trying with other interface types
			if (VERBOSE)
			{
				System.err.println("Could not create calling SICStus interface:");
				e.printStackTrace();
			}
		}
		
		// try other interface types here
		
		return null;
	}

}
