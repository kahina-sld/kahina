package org.kahina.tralesld;

import java.io.File;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaSessionEvent;
import org.kahina.tralesld.bridge.TraleSLDBridge;

public class TraleSLDRunner extends KahinaRunner
{

	public static void initialize()
	{
		KahinaRunner.initialize();
	}

	public static TraleSLDBridge runAndGetBridge()
	{
		try
		{
			initialize();
			TraleSLDInstance kahina = new TraleSLDInstance();
			kahina.getGUI().prepare(KahinaRunner.getControl());
			kahina.getGUI().show();
			return kahina.getBridge();
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
			TraleSLDInstance kahina = new TraleSLDInstance();
			kahina.getGUI().prepare(KahinaRunner.getControl());
			kahina.getGUI().show();
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
