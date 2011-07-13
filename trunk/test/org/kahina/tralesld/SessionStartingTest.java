package org.kahina.tralesld;

public class SessionStartingTest
{
	
	public static void main(String[] args)
	{
		TraleSLDInstance instance = new TraleSLDInstance();
		instance.startNewSession();
		// Should not lead to the creation of a new GUI, but to clearing the old
		// one:
		instance.startNewSession();
	}

}
