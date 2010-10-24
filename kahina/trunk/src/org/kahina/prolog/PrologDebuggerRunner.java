package org.kahina.prolog;

import java.io.File;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaSessionEvent;
import org.kahina.prolog.bridge.PrologBridge;

public class PrologDebuggerRunner extends KahinaRunner
{

	public static void initialize()
	{
		KahinaRunner.initialize();
	}

	public static PrologBridge runAndGetBridge()
	{
		try
		{
			initialize();
			PrologDebuggerInstance instance = new PrologDebuggerInstance();
			instance.getGUI().prepare(KahinaRunner.getControl());
			instance.getGUI().show();
			return instance.getBridge();
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
			return null;
		}
	}

	private static void runWithoutBridge(File savedSession)
	{
		try
		{
			initialize();
			PrologDebuggerInstance instance = new PrologDebuggerInstance();
			instance.getGUI().prepare(KahinaRunner.getControl());
			instance.getGUI().show();
			if (savedSession != null)
			{
				KahinaRunner.processEvent(new KahinaSessionEvent(KahinaSessionEvent.LOAD_SESSION, savedSession));
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}
	
	public static void main(String[] args)
	{
		File savedSession = null;
		if (args.length > 0)
		{
			savedSession = new File(args[0]);
		}
		runWithoutBridge(savedSession);
	}
}
