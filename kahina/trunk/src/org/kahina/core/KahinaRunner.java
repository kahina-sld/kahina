package org.kahina.core;

import java.io.File;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.io.magazine.ObjectMagazine;

public class KahinaRunner
{
	private static final boolean VERBOSE = false;
	
    private static ObjectMagazine<KahinaStep> steps;
    private static KahinaController control;
    
    public static void initialize()
    {
    	control = new KahinaController();
    	steps = ObjectMagazine.create();
    }

	public static void deinitialize()
	{
		if (VERBOSE)
		{
			System.err.println("KahinaRunner ist deinitializing.");
		}
		steps.close();
		steps = null;
		control = null;
	}
    
    public static void processEvent(KahinaEvent e)
    {
        control.processEvent(e);
    }
    
    public static KahinaController getControl()
    {
        return control;
    }

	public static void store(int id, KahinaObject object)
	{
		steps.store(id, (KahinaStep) object);
	}

	@SuppressWarnings("unchecked")
	public static <T extends KahinaObject> T retrieve(Class<T> type, int stepID)
	{
		// TODO we want to do this differently
		return (T) steps.retrieve(stepID);
	}

	public static ObjectMagazine<KahinaStep> getSteps()
	{
		return steps;
	}

	public static void loadSteps(File directory)
	{
		steps = ObjectMagazine.load(directory, KahinaStep.class);
	}
}
