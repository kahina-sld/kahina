package org.kahina.core;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;

public class KahinaDefaultInstance extends KahinaInstance
{
	@Override
	protected KahinaBridge createBridge() 
	{
		return new KahinaBridge(this);
	}

	@Override
	protected KahinaGUI createGUI(KahinaController guiController) 
	{
		return null;
	}

	@Override
	protected KahinaState createState() 
	{
		return null;
	}

	@Override
	protected void createTreeBehavior() 
	{
		// TODO Auto-generated method stub
		
	}

}
