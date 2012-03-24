package org.kahina.core;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.gui.KahinaGUI;

public class KahinaDefaultInstance extends KahinaInstance<KahinaState, KahinaGUI, KahinaBridge>
{
	@Override
	protected KahinaBridge createBridge() 
	{
		return new KahinaBridge(this);
	}

	@Override
	protected KahinaGUI createGUI() 
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
