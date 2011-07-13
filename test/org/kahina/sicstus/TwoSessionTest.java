package org.kahina.sicstus;

import org.kahina.sicstus.bridge.SICStusPrologBridge;

public class TwoSessionTest
{

	public static void main(String args[]) throws InterruptedException
	{
		SICStusPrologDebuggerInstance i = new SICStusPrologDebuggerInstance();
		SICStusPrologBridge b = i.startNewSession();
		while (b.getAction() != 'a') // wait for user to close GUI
		{
			Thread.sleep(100);
		}
		i.startNewSession();
	}

}
