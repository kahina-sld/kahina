package org.kahina.core;

import java.io.File;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.io.magazine.ObjectMagazine;

public class KahinaRunner // TODO get rid of this class, make everything non-static
{
	private static final boolean VERBOSE = false;
	
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
