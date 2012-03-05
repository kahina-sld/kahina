package org.kahina.core;

import java.io.File;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.io.magazine.ObjectMagazine;

public class KahinaRunner // TODO get rid of this class, make everything non-static
{
	private static final boolean VERBOSE = false;
	
    private static ObjectMagazine<KahinaStep> steps;
    private static KahinaController control = new KahinaController();
    private static KahinaController guiControl;
    
    public static void processEvent(KahinaEvent e)
    {
    	if (VERBOSE)
    	{
    		System.err.println("KahinaRunner.processEvent(" + e + ")");
    	}
    	guiControl.processEvent(e);
        control.processEvent(e);
    }
    
    public static KahinaController getControl()
    {
        return control;
    }
    
    public static void setControl(KahinaController control)
    {
    	KahinaRunner.control = control;
    }

    public static void setSteps(ObjectMagazine<KahinaStep> steps)
    {
    	KahinaRunner.steps = steps;
    }
    
	public static void store(int id, KahinaObject object)
	{
		if (VERBOSE)
		{
			System.err.println("KahinaRunner.store(" + id + "," + object + ")");
			System.err.println("steps == " + steps);
		}
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

	public static void setGUIController(KahinaController guiControl)
	{
		KahinaRunner.guiControl = guiControl;
	}
	
	public static KahinaController getGUIControl()
	{
		return guiControl;
	}

	public static boolean isInitialized()
	{
		return control != null;
	}
}
